;;; elfeed-db.el --- database and model for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Elfeed is aware of two type of things: feeds and entries. All dates
;; are stored as floating point epoch seconds.

;; Feeds are keyed by their user-provided feed URL, which acts as the
;; feed identity regardless of any other stated identity. Feeds have a
;; list of entries.

;; Entries are keyed in order of preference by id (Atom), guid (RSS),
;; or link. To avoid circular references, entries refer to their
;; parent feeds by URL.

;; Feed content is stored in a content-addressable loose-file
;; database, very similar to an unpacked Git object database. Entries
;; have references to items in this database (elfeed-ref), keeping the
;; actual entry struct memory footprint small. Most importantly, this
;; keeps the core index small so that it can quickly be written as a
;; whole to the filesystem. The wire format is just the s-expression
;; print form of the top-level hash table.

;; An AVL tree containing all database entries ordered by date is
;; maintained as part of the database. We almost always want to look
;; at entries ordered by date and this step accomplished that very
;; efficiently with the AVL tree. This is the reasoning behind the
;; `with-elfeed-db-visit' interface.

;; Unfortunately there's a nasty bug (bug#15190) in the reader that
;; makes hash tables and `print-circle' incompatible. It's been fixed
;; in trunk, but many users will likely be stuck with this bug for the
;; next few years. This means the database format can't exploit
;; circular references.

;;; Code:

(require 'cl)
(require 'avl-tree)
(require 'elfeed-lib)

(defcustom elfeed-db-directory "~/.elfeed"
  "Directory where elfeed will store its database."
  :group 'elfeed
  :type 'directory)

(defvar elfeed-db nil
  "The core database for elfeed.")

(defvar elfeed-db-feeds nil
  "Feeds hash table, part of `elfeed-db'.")

(defvar elfeed-db-entries nil
  "Entries hash table, part of `elfeed-db'.")

(defvar elfeed-db-index nil
  "Collection of all entries sorted by date, part of `elfeed-db'.")

(defvar elfeed-db-version "0.0.1"
  "The database version this version of Elfeed expects to use.")

(defvar elfeed-new-entry-hook ()
  "Functions in this list are called with the new entry as its
argument. This is a chance to add cutoms tags to new entries.")

(defvar elfeed-db-update-hook ()
  "Functions in this list are called with no arguments any time
the :last-update time is updated.")

;; Data model:

(defstruct elfeed-feed
  "A web feed, contains elfeed-entry structs."
  id url title author)

(defstruct elfeed-entry
  "A single entry from a feed, normalized towards Atom."
  id title link date content content-type enclosures tags feed-id)

(defun elfeed-entry-merge (a b)
  "Merge B into A, preserving A's tags. Return true if an actual
update occurred, not counting content."
  (setf (elfeed-entry-tags b) (elfeed-entry-tags a)
        (elfeed-entry-content a) (elfeed-entry-content b))
  (not
   (zerop
    (loop for i from 0 below (length a)
          for part-a = (aref a i)
          for part-b = (aref b i)
          count (not (equal part-a part-b))
          do (setf (aref a i) part-b)))))

(defun elfeed-db-get-feed (id)
  "Get/create the feed for ID."
  (elfeed-db-ensure)
  (let ((feed (gethash id elfeed-db-feeds)))
    (or feed
        (setf (gethash id elfeed-db-feeds)
              (make-elfeed-feed :id id)))))

(defun elfeed-db-get-entry (id)
  "Get the entry for ID."
  (elfeed-db-ensure)
  (gethash id elfeed-db-entries))

(defun elfeed-db-compare (a b)
  "Return true if entry A is newer than entry B."
  (let* ((entry-a (elfeed-db-get-entry a))
         (entry-b (elfeed-db-get-entry b))
         (date-a (elfeed-entry-date entry-a))
         (date-b (elfeed-entry-date entry-b)))
    (if (= date-a date-b)
        (string< (prin1-to-string b) (prin1-to-string a))
      (> date-a date-b))))

(defun elfeed-db-set-update-time ()
  "Update the database last-update time."
  (plist-put elfeed-db :last-update (float-time))
  (run-hooks 'elfeed-db-update-hook))

(defun elfeed-db-add (entries)
  "Add ENTRIES to the database."
  (elfeed-db-ensure)
  (loop for entry in entries
        for id = (elfeed-entry-id entry)
        for original = (gethash id elfeed-db-entries)
        for new-date = (elfeed-entry-date entry)
        for original-date = (and original (elfeed-entry-date original))
        do (elfeed-deref-entry entry)
        when original count
          (if (= new-date original-date)
              (elfeed-entry-merge original entry)
            (avl-tree-delete elfeed-db-index id)
            (prog1 (elfeed-entry-merge original entry)
              (avl-tree-enter elfeed-db-index id)))
          into change-count
        else count
          (setf (gethash id elfeed-db-entries) entry)
          into change-count
        and do
          (progn
            (avl-tree-enter elfeed-db-index id)
            (loop for hook in elfeed-new-entry-hook
                  do (funcall hook entry)))
        finally
          (unless (zerop change-count)
            (elfeed-db-set-update-time)))
  :success)

(defun elfeed-entry-feed (entry)
  "Get the feed struct for ENTRY."
  (elfeed-db-get-feed (elfeed-entry-feed-id entry)))

(defun elfeed-normalize-tags (tags)
  "Return the normalized tag list for TAGS."
  (remove-duplicates (sort* (copy-seq tags) #'string< :key #'symbol-name)))

(defun elfeed-tag (entry &rest tags)
  "Add TAGS to ENTRY."
  (let ((current (elfeed-entry-tags entry)))
    (setf (elfeed-entry-tags entry)
          (elfeed-normalize-tags (append tags current)))))

(defun elfeed-untag (entry &rest tags)
  "Remove TAGS from ENTRY."
  (setf (elfeed-entry-tags entry)
        (loop for tag in (elfeed-entry-tags entry)
              unless (member tag tags) collect tag)))

(defun elfeed-tagged-p (tag entry)
  "Return true if ENTRY is tagged by TAG."
  (member tag (elfeed-entry-tags entry)))

(defun elfeed-db-last-update ()
  "Return the last database update time in (`float-time') seconds."
  (elfeed-db-ensure)
  (or (plist-get elfeed-db :last-update) 0))

(defmacro with-elfeed-db-visit (entry-and-feed &rest body)
  "Visit each entry in the database from newest to oldest.
Use `elfeed-db-return' to exit early and optionally return data.

  (with-elfeed-db-visit (entry feed)
    (do-something entry)
    (when (some-date-criteria-p entry)
      (elfeed-db-return)))"
  (declare (indent defun))
  `(catch 'elfeed-db-done
     (prog1 nil
       (elfeed-db-ensure)
       (avl-tree-mapc
        (lambda (id)
          (let* ((,(first entry-and-feed) (elfeed-db-get-entry id))
                 (,(second entry-and-feed)
                  (elfeed-entry-feed ,(first entry-and-feed))))
            ,@body))
        elfeed-db-index))))

(defun elfeed-apply-hooks-now ()
  "Apply `elfeed-new-entry-hook' to all entries in the database."
  (interactive)
  (with-elfeed-db-visit (entry _)
    (loop for hook in elfeed-new-entry-hook
          do (funcall hook entry))))

(defmacro elfeed-db-return (&optional value)
  "Use this to exit early and return VALUE from `with-elfeed-db-visit'."
  `(throw 'elfeed-db-done ,value))

;; Saving and Loading:

(defun elfeed-db-save ()
  "Write the database index to the filesystem."
  (elfeed-db-ensure)
  (mkdir elfeed-db-directory t)
  (with-temp-file (expand-file-name "index" elfeed-db-directory)
    (set-buffer-multibyte nil)
    (let ((standard-output (current-buffer)))
      (prin1 elfeed-db)
      :success)))

(defun elfeed-db-load ()
  "Load the database index from the filesystem."
  (let ((index (expand-file-name "index" elfeed-db-directory)))
    (if (not (file-exists-p index))
        (let ((db (setf elfeed-db (list :version elfeed-db-version))))
          (plist-put db :feeds (make-hash-table :test 'equal))
          (plist-put db :entries (make-hash-table :test 'equal))
          (plist-put db :index (avl-tree-create #'elfeed-db-compare)))
      (with-current-buffer (find-file-noselect index)
        (goto-char (point-min))
        (setf elfeed-db (read (current-buffer)))
        (kill-buffer)))
    (setf elfeed-db-feeds (plist-get elfeed-db :feeds)
          elfeed-db-entries (plist-get elfeed-db :entries)
          elfeed-db-index (plist-get elfeed-db :index)
          ;; Internal function use required for security!
          (avl-tree--cmpfun elfeed-db-index) #'elfeed-db-compare)))

(defun elfeed-db-ensure ()
  "Ensure that the database has been loaded."
  (when (null elfeed-db) (elfeed-db-load)))

(defun elfeed-db-size ()
  "Return a count of the number of entries in the database."
  (let ((count-table (hash-table-count elfeed-db-entries))
        (count-tree (avl-tree-size elfeed-db-index)))
    (if (= count-table count-tree)
        count-table
      (error "Elfeed database error: entry count mismatch."))))

;; Filesystem storage:

(defstruct elfeed-ref
  id)

(defun elfeed-ref--file (ref)
  "Determine the storage filename for REF."
  (let* ((id (elfeed-ref-id ref))
         (root (expand-file-name "data" elfeed-db-directory))
         (subdir (expand-file-name (substring id 0 2) root)))
    (expand-file-name id subdir)))

(defun elfeed-ref-exists-p (ref)
  "Return true if REF can be dereferenced."
  (file-exists-p (elfeed-ref--file ref)))

(defun elfeed-deref (ref)
  "Fetch the content behind the reference, or nil if non-existent."
  (if (not (elfeed-ref-p ref))
      ref
    (let ((file (elfeed-ref--file ref)))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))))))

(defun elfeed-ref (content)
  "Create a reference to CONTENT, to be persistently stored."
  (if (elfeed-ref-p content)
      content
    (let* ((id (secure-hash 'sha1 (encode-coding-string content 'utf-8 t)))
           (ref (make-elfeed-ref :id id))
           (file (elfeed-ref--file ref)))
      (prog1 ref
        (unless (file-exists-p file)
          (mkdir (file-name-directory file) t)
          (let ((coding-system-for-write 'utf-8))
            (with-temp-file file
              (insert content))))))))

(defun elfeed-deref-entry (entry)
  "Move ENTRY's content to filesystem storage. Return the entry."
  (let ((content (elfeed-entry-content entry)))
    (prog1 entry
      (when (stringp content)
        (setf (elfeed-entry-content entry) (elfeed-ref content))))))

(defun elfeed-ref-delete (ref)
  "Remove the content behind REF from the database."
  (delete-file (elfeed-ref--file ref)))

(defun elfeed-db-gc (&optional stats-p)
  "Clean up unused content from the content database. If STATS is
true, return the space cleared in bytes."
  (let* ((data (expand-file-name "data" elfeed-db-directory))
         (dirs (cddr (directory-files data t)))
         (ids (mapcan (lambda (d) (directory-files d nil nil t)) dirs))
         (table (make-hash-table :test 'equal)))
    (dolist (id ids)
      (setf (gethash id table) nil))
    (with-elfeed-db-visit (entry _)
      (let ((content (elfeed-entry-content entry)))
        (when (elfeed-ref-p content)
          (setf (gethash (elfeed-ref-id content) table) t))))
    (loop for id hash-keys of table using (hash-value used)
          for used-p = (or used (member id '("." "..")))
          when (and (not used-p) stats-p)
          sum (let* ((ref (make-elfeed-ref :id id))
                     (file (elfeed-ref--file ref)))
                (* 1.0 (nth 7 (file-attributes file))))
          unless used-p
          do (elfeed-ref-delete (make-elfeed-ref :id id)))))

(defun elfeed-db-gc-safe ()
  "Run `elfeed-db-gc' without triggering any errors, for use as a safe hook."
  (ignore-errors (elfeed-db-gc-safe)))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'elfeed-db-gc-safe :append)
  (add-hook 'kill-emacs-hook #'elfeed-db-save))

(provide 'elfeed-db)

;;; elfeed-db.el ends here
