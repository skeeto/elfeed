;;; elfeed-db.el --- database and model for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Elfeed is aware of two type of things: feeds and entries.

;; Feeds are keyed by their user-provided feed URL, which acts as the
;; feed identity regardless of any other stated identity. Feeds have a
;; list of entries.

;; Entries are keyed in order of preference by id (Atom), guid (RSS),
;; or link. To avoid circular references, entries refer to their
;; parent feeds by URL.

;;; Code:

(require 'cl)
(require 'elfeed-lib)

(defcustom elfeed-db-directory "~/.elfeed"
  "Directory where elfeed will store its database."
  :group 'elfeed
  :type 'directory)

(defvar elfeed-db nil
  "The core database for elfeed.")

(defvar elfeed-new-entry-hook ()
  "Functions in this list are called with the new entry as its
argument. This is a chance to add cutoms tags to new entries.")

;; Data model:

(defstruct elfeed-feed
  "A web feed, contains elfeed-entry structs."
  title url entries)

(defstruct elfeed-entry
  "A single entry from a feed, normalized towards Atom."
  title id link date content content-type tags feed-url)

(defun elfeed-db-get (url)
  "Get/create the FEED for URL."
  (elfeed-db-ensure)
  (let ((feed (gethash url elfeed-db)))
    (or feed
        (setf (gethash url elfeed-db)
              (make-elfeed-feed
               :url url :entries (make-hash-table :test 'equal))))))

(defun elfeed-db-put (url entries)
  "Add entries to the database under URL."
  (elfeed-db-ensure)
  (let* ((feed (elfeed-db-get url))
         (table (elfeed-feed-entries feed)))
    (loop for entry in entries
          for id = (elfeed-entry-id entry)
          for old = (gethash id table)
          do (elfeed-deref-entry entry)
          when old  ; merge old tags back in
          do (setf (elfeed-entry-tags entry) (elfeed-entry-tags old))
          when (not old)
          do (loop for hook in elfeed-new-entry-hook
                   do (funcall hook entry))
          do (setf (gethash id table) entry))
    (prog1 (setf (gethash :last-update elfeed-db) (float-time))
      (elfeed-db-save))))

(defun elfeed-entry-feed (entry)
  "Get the feed struct for ENTRY."
  (elfeed-db-get (elfeed-entry-feed-url entry)))

(defun elfeed-tag (entry &rest tags)
  "Add TAGS to ENTRY."
  (let ((current (elfeed-entry-tags entry)))
    (setf (elfeed-entry-tags entry) (remove-duplicates (append tags current)))))

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
  (gethash :last-update elfeed-db 0))

(defun elfeed-sort (entries &optional old-first)
  "Destructively sort the given entries by date."
  (sort* entries (if old-first #'< #'>)
         :key #'elfeed-entry-date))

(defun elfeed-db-entries (&optional url)
  "Get all the entries, optionally just for URL, sorted by date."
  (elfeed-sort
   (if (null url)
       (loop for url in elfeed-feeds
             append (elfeed-db-entries url))
     (loop for entry hash-values of (elfeed-feed-entries (elfeed-db-get url))
           collect entry))))

(defun elfeed-apply-hooks-now ()
  "Apply `elfeed-new-entry-hook' to all entries in the database."
  (interactive)
  (loop with entries = (elfeed-db-entries)
        for hook in elfeed-new-entry-hook
        do (mapc hook entries)))

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
          (set-buffer-multibyte t)
          (insert-file-contents file)
          (buffer-string))))))

(defun elfeed-ref (content)
  "Create a reference to CONTENT, to be persistently stored."
  (if (elfeed-ref-p content)
      content
    (let* ((id (secure-hash 'sha1 content))
           (ref (make-elfeed-ref :id id))
           (file (elfeed-ref--file ref)))
      (prog1 ref
        (unless (file-exists-p file)
          (mkdir (file-name-directory file) t)
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert content)))))))

(defun elfeed-deref-entry (entry)
  "Move ENTRY's content to filesystem storage. Return the entry."
  (let ((content (elfeed-entry-content entry)))
    (prog1 entry
      (when (stringp content)
        (setf (elfeed-entry-content entry) (elfeed-ref content))))))

(defun elfeed-db-save ()
  "Write the database index to the filesystem."
  (mkdir elfeed-db-directory t)
  (with-temp-file (expand-file-name "index" elfeed-db-directory)
    (let ((standard-output (current-buffer)))
      (prin1 elfeed-db)
      :success)))

(defun elfeed-db-load ()
  "Load the database index from the filesystem."
  (let ((index (expand-file-name "index" elfeed-db-directory)))
    (if (not (file-exists-p index))
        (setq elfeed-db (make-hash-table :test 'equal))
      (with-current-buffer (find-file-noselect index)
        (goto-char (point-min))
        (setq elfeed-db (read (current-buffer)))
        (kill-buffer)))))

(defun elfeed-db-ensure ()
  "Ensure that the database has been loaded."
  (when (null elfeed-db) (elfeed-db-load)))

(provide 'elfeed-db)

;;; elfeed-db.el ends here
