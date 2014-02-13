;;; elfeed-emacsql.el --- Emacsql database back-end -*- lexical-binding: t; -*-

;; Elfeed is aware of two type of things: feeds and entries. All dates
;; are stored as floating point epoch seconds.

;; All data structures are keyed by a UUID string. In the case of
;; Atom, the provided UUID (id tag) is used. RSS has no sufficiently
;; unique idetifiers, so UUIDs are generated (`elfeed-uuid') for feeds
;; and entries based on URLs (falling back to other values if not
;; present).

;; The backend is a SQLite database. The functions `elfeed-save' (and
;; family), `elfeed-tag', and `elfeed-untag' are where updates to the
;; database are actually written.

;; Entry and feed objects can have arbitrary metadata attached. The
;; setf-able `elfeed-meta' function is used to access these. Currently
;; this data is not stored in the database, but it may be someday.

;;; Code:

(require 'cl-lib)
(require 'emacsql)

(defcustom elfeed-db-directory "~/.elfeed"
  "Directory where elfeed will store its database.
This variable is obsolete. Use `elfeed-db-file' instead."
  :group 'elfeed
  :type 'directory)

(defcustom elfeed-db-file
  (expand-file-name "elfeed.sqlite" elfeed-db-directory)
  "File name of the SQLite database."
  :group 'elfeed
  :type 'directory)

(defvar elfeed-new-entry-hook ()
  "Functions in this list are called with the new entry as its
argument. This is a chance to add cutoms tags to new entries.")

(defvar elfeed-db-update-hook ()
  "Functions in this list are called with no arguments any time
the database is updated.")

;; Data Model:

(cl-defstruct (elfeed-thing (:constructor elfeed-thing--create))
  "Parent struct for all Elfeed structs."
  plist)

(cl-defstruct (elfeed-feed (:constructor elfeed-feed--create)
                           (:include elfeed-thing))
  "A web feed, contains elfeed-entry structs."
  id link title author)

(cl-defstruct (elfeed-entry (:constructor elfeed-entry--create)
                            (:include elfeed-thing))
  "A single entry from a feed, normalized towards Atom."
  id feed-id link title date -content -content-type enclosures tags)

;; Database Schema:

(defvar elfeed-sql-schema-feeds
 '([(feed-id :primary-key :not-null)
    (feed-link :not-null)
    feed-title
    feed-author]))

(defvar elfeed-sql-schema-entries
  '([(entry-id :primary-key :not-null)
     feed-id
     entry-link
     entry-title
     (entry-date float :not-null)]
    (:foreign-key [feed-id] :references feeds [feed-id]
                  :on-delete :cascade)))

(defvar elfeed-sql-schema-content
  '([(entry-id :primary-key :not-null)
     content
     content-type]
    (:foreign-key [entry-id] :references entries [entry-id]
                  :on-delete :cascade)))

(defvar elfeed-sql-schema-tags
  '([(entry-id :not-null)
     (tag :not-null)]
    (:foreign-key [entry-id] :references entries [entry-id]
                  :on-delete :cascade)
    (:primary-key [entry-id tag])))

(defvar elfeed-sql-schema-enclosures
  '([(entry-id :not-null)
     enclosure]
    (:foreign-key [entry-id] :references entries [entry-id]
                  :on-delete :cascade)
    (:primary-key [entry-id enclosure])))

;; Connection Management:

(defvar elfeed-sql--connection nil
  "Stores the EmacSQL database connection.")

(defun elfeed-db-last-update ()
  "Return the last database update time in (`float-time') seconds."
  (float-time (elt (file-attributes elfeed-db-file) 5)))

(defun elfeed-sql-db ()
  "Ensure that a database connection has been established and return it."
  (if (and elfeed-sql--connection (emacsql-live-p elfeed-sql--connection))
      elfeed-sql--connection
    (prog1 (setf elfeed-sql--connection (emacsql-sqlite elfeed-db-file))
      (elfeed-sql-init))))

(defun elfeed-sql-init ()
  "Initialize the Elfeed SQLite database."
  (emacsql-with-transaction (elfeed-sql-db)
    (cl-mapc (apply-partially
              #'emacsql (elfeed-sql-db)
              [:create-table :if-not-exists $i1 $S2])
             '(feeds entries content tags enclosures)
             (list elfeed-sql-schema-feeds
                   elfeed-sql-schema-entries
                   elfeed-sql-schema-content
                   elfeed-sql-schema-tags
                   elfeed-sql-schema-enclosures))))

(defun elfeed-sql-drop! ()
  "Drop all tables in the SQLite database."
  (emacsql-with-transaction (elfeed-sql-db)
    (mapc (apply-partially #'emacsql (elfeed-sql-db) [:drop-table $i1])
          '(enclosures tags content entries feeds)))
  (emacsql-close elfeed-sql--connection))

(defun elfeed-sql (sql &rest args)
  "Run SQL expression on the database."
  (apply #'emacsql (elfeed-sql-db) sql args))

(defmacro elfeed-with-transaction (&rest body)
  "Evaluate BODY within a single transaction."
  (declare (indent 0))
  `(emacsql-with-transaction (elfeed-sql-db) ,@body))

;; Struct Metadata:

(defun elfeed-meta (thing key)
  "Access metadata for THING (entry, feed) under KEY."
  (plist-get (elfeed-thing-plist thing) key))

(defun elfeed-meta--put (thing key value)
  "Set metadata to VALUE on THING under KEY."
  (when (not (elfeed-readable-p value)) (error "New value must be readable."))
  (prog1 value
    (setf (elfeed-thing-plist thing)
          (plist-put (elfeed-thing-plist thing) key value))))

(gv-define-simple-setter elfeed-meta elfeed-meta--put)

;; Feed Access:

(defun elfeed-get-feed (feed-id &optional url)
  "Get feed by FEED-ID, creating it if it doesn't exist and URL provided."
  (let ((result (emacsql (elfeed-sql-db)
                         [:select [feed-id feed-link feed-title feed-author]
                          :from feeds :where (= feed-id $s1)]
                         feed-id)))
    (cond (result (vconcat (list 'cl-struct-elfeed-feed nil) (car result)))
          (url (elfeed-feed--create :id feed-id :link url)))))

(defun elfeed-get-feed-by-url (url &optional new-feed-id)
  "Get feed by URL, creating it if it doesn't exist and NEW-FEED-ID provided."
  (let ((feed-id (caar (emacsql (elfeed-sql-db)
                                [:select [feed-id] :from feeds
                                 :where (= feed-link $s1)]
                                url))))
    (cond (feed-id (elfeed-get-feed feed-id))
          (new-feed-id (elfeed-feed--create :id new-feed-id :link url)))))

(defun elfeed-get-feed-entries (feed-id)
  "Return all entry-ids belonging to FEED-ID."
  (mapcar #'car (elfeed-sql [:select [entry-id] :from entries
                             :where (= feed-id $s1)
                             :order-by [(desc entry-date)]]
                            feed-id)))

(defun elfeed-feed-entries (feed)
  "Return all entries belonging to FEED."
  (mapcar #'elfeed-get-entry (elfeed-get-feed-entries (elfeed-feed-id feed))))

;; Entry Access:

(defun elfeed-get-entry-exists-p (entry-id)
  "Return non-nil of ENTRY-ID refers to an existing database row."
  (emacsql (elfeed-sql-db) [:select [entry-id] :from entries
                            :where (= entry-id $s1)] entry-id))

(defun elfeed-get-entry-tags (entry-id)
  (mapcar #'car (emacsql (elfeed-sql-db)
                         [:select [tag] :from tags
                          :where (= entry-id $s1)
                          :order-by [tag]]
                         entry-id)))

(defun elfeed-get-entry-enclosures (entry-id)
  (mapcar #'car (emacsql (elfeed-sql-db)
                         [:select [enclosure] :from enclosures
                          :where (= entry-id $s1)]
                         entry-id)))

(defun elfeed-get-entry-content (entry-id)
  (or (car (emacsql (elfeed-sql-db)
                    [:select [content content-type] :from content
                     :where (= entry-id $s1)]
                    entry-id))
      (list nil nil)))

(defun elfeed-get-entry (entry-id)
  "Get a feed by its ENTRY-ID."
  (let ((db (elfeed-sql-db)))
    (emacsql-with-transaction db
      (let ((entry
             (emacsql db [:select
                          [entry-id feed-id entry-link entry-title entry-date]
                          :from entries :where (= entry-id $s1)]
                      entry-id)))
        (when entry
          (vconcat (list 'cl-struct-elfeed-entry nil)
                   (car entry) (list :unbound :unbound)
                   (list (elfeed-get-entry-enclosures entry-id))
                   (list (elfeed-get-entry-tags entry-id))))))))

(defun elfeed--fill-entry-content (entry)
  "Fill in the content fields for ENTRY."
  (prog1 entry
    (let ((content (elfeed-entry--content entry)))
      (when (eq content :unbound)
        (cl-destructuring-bind (content type)
            (elfeed-get-entry-content (elfeed-entry-id entry))
          (setf (elfeed-entry--content      entry) content
                (elfeed-entry--content-type entry) type))))))

(defun elfeed-entry-content (entry)
  "Get the content for ENTRY lazily."
  (elfeed--fill-entry-content entry)
  (elfeed-entry--content entry))

(defun elfeed-entry-content-type (entry)
  "Get the content-type for ENTRY lazily."
  (elfeed--fill-entry-content entry)
  (elfeed-entry--content-type entry))

(defun elfeed-entry-feed (entry)
  "Get the feed struct for ENTRY."
  (elfeed-get-feed (elfeed-entry-feed-id entry)))

;; Entry Tagging:

(defun elfeed-tag (entry &rest tags)
  "Add TAGS to ENTRY."
  (let ((entry-id (elfeed-entry-id entry)))
    (elfeed-sql [:replace-into tags [entry-id tag] :values $v1]
                (mapcar (apply-partially #'vector entry-id) tags))
    (setf (elfeed-entry-tags entry)
          (cl-union (elfeed-entry-tags entry) tags))))

(defun elfeed-untag (entry &rest tags)
  "Remove TAGS from ENTRY."
  (elfeed-sql [:delete-from tags :where (and (= entry-id $s1) (in tag $v2))]
              (elfeed-entry-id entry) tags)
  (setf (elfeed-entry-tags entry)
        (cl-set-difference (elfeed-entry-tags entry) tags)))

(defun elfeed-tagged-p (tag entry)
  "Return non-nil if ENTRY is tagged by TAG in the database."
  (not (null (elfeed-sql [:select [entry-id] :from tags
                          :where (and (= entry-id $s1) (= tag $s2))]
                         (elfeed-entry-id entry) tag))))

;; Database Writeback:

(defun elfeed-feed-save (feed)
  "Update FEED in the database, returning FEED."
  (prog1 feed
    (emacsql (elfeed-sql-db)
             [:replace-into feeds [feed-id feed-link feed-title feed-author]
              :values $v1]
             (subseq feed 2 6))))

(defun elfeed-entry-save (entry)
  "Insert or update ENTRY in the database, returning ENTRY."
  (let ((db (elfeed-sql-db))
        (entry-id (elfeed-entry-id entry)))
    (emacsql-with-transaction db
      (emacsql (elfeed-sql-db)
               [:replace-into entries
                [entry-id feed-id entry-link entry-title entry-date]
                :values $v1]
               (subseq entry 2 7))
      (let* ((db-tags (elfeed-get-entry-tags entry-id))
             (tags (elfeed-entry-tags entry))
             (remove (cl-set-difference db-tags tags))
             (add (cl-set-difference tags db-tags)))
        (dolist (tag remove)
          (emacsql db [:delete-from tags
                       :where (and (= entry-id $s1) (= tag $s2))]
                   entry-id tag))
        (dolist (tag add)
          (emacsql db [:insert-into tags [entry-id tag] :values $v1]
                   (vector entry-id tag))))
      (let* ((db-enclosures (elfeed-get-entry-enclosures entry-id))
             (enclosures (elfeed-entry-enclosures entry))
             (remove (cl-set-difference db-enclosures enclosures))
             (add (cl-set-difference enclosures db-enclosures)))
        (dolist (enclosure remove)
          (emacsql db [:delete-from enclosures
                       :where (and (= entry-id $s1) (= enclosure $s2))]
                   entry-id enclosure))
        (dolist (enclosure add)
          (emacsql db [:insert-into enclosures [entry-id enclosure]
                       :values $v1]
                   (vector entry-id enclosure))))
      (cond ((stringp (elfeed-entry--content entry))
             (emacsql db [:replace-into content
                          [entry-id content content-type]
                          :values $v1]
                      (vector entry-id
                              (elfeed-entry-content entry)
                              (elfeed-entry-content-type entry))))
            ((null (elfeed-entry--content entry))
             (emacsql db [:delete-from content :where (= entry-id $s1)]
                      entry-id)))
      entry)))

(defun elfeed-save (thing)
  "Insert or update THING in database."
  (cl-etypecase thing
    (elfeed-entry (elfeed-entry-save thing))
    (elfeed-feed  (elfeed-feed-save  thing))))

(defun elfeed-feed-delete (feed)
  "Delete FEED (or feed id) from the database.
Warning: this also deletes all entries belonging to FEED."
  (let ((feed-id (if (elfeed-feed-p feed) (elfeed-feed-id feed) feed)))
    (emacsql (elfeed-sql-db) [:delete-from feeds :where (= feed-id $s1)]
             feed-id)))

(defun elfeed-entry-delete (entry)
  "Delete FEED (or feed id) from the database."
  (let ((entry-id (if (elfeed-entry-p entry) (elfeed-entry-id entry) entry)))
    (emacsql (elfeed-sql-db) [:delete-from entries :where (= entry-id $s1)]
             entry-id)))

(defun elfeed-delete (thing)
  "Delete THING from the database."
  (cl-etypecase thing
    (elfeed-entry (elfeed-entry-delete thing))
    (elfeed-feed  (elfeed-feed-delete  thing))))

;; General Database Queries:

(cl-defun elfeed-find
    (&key (required-tags () required-p) prohibited-tags (since -1e32))
  "Return a list of all entry-ids, ordered by date, which meet the criteria.
Given no criteria, return *all* entries.

:required-tags -- if provided, the entry must have all of these tags
:prohibited-tags -- entry must *not* have any of these tags
:since -- entry must have a timestamp at or after this date (float)"
  (let ((required (vconcat required-tags))
        (prohibited (vconcat prohibited-tags)))
    (mapcar #'car (elfeed-sql
                   [:select [entry-id]
                    :from entries :natural-join tags
                    :where (and (>= entry-date $s1)
                                (or (not $s2)
                                    (in tag $v3)) ; XXX not quite right
                                (not (in tag $v4)))
                    :group-by entry-id
                    :order-by [(desc entry-date)]]
                   since (if required-p 1 0) required prohibited))))

(defun elfeed-db-size ()
  "Return a count of the number of entries in the database."
  (caar (elfeed-sql [:select (funcall count *) :from entries])))

;; Misc:

(defun elfeed-db-vacuum ()
  "Compact the database file as much as possible."
  (elfeed-sql [:vacuum]))

(defun elfeed-apply-hooks-now ()
  "Apply `elfeed-new-entry-hook' to all entries in the database."
  (interactive)
  (let ((entries (mapcar #'elfeed-get-entry (elfeed-find))))
    (dolist (hook elfeed-new-entry-hook)
      (mapc hook entries))))

;; Old Import:

(require 'elfeed)

(defun elfeed-legacy-lookup (url)
  "Determine the canonical UUID for the feed at URL."
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((beg (1+ (marker-position url-http-end-of-headers)))
                 (xml (xml-parse-region beg (point-max)))
                 (type (elfeed-feed-type xml)))
            (or (and (eq type :atom) (xml-query '(feed id *) xml))
                (elfeed-uuid url))))
      (kill-buffer buffer))))

(defun elfeed-legacy-import ()
  "Load everything from the old Elfeed database."
  (let* ((index (expand-file-name "index" elfeed-db-directory))
         (db (with-current-buffer (find-file-noselect index)
               (setf (point) (point-min))
               (prog1 (read (current-buffer))
                 (kill-buffer))))
         (feeds-hash (plist-get db :feeds))
         (entries-hash (plist-get db :entries)))
    (elfeed-with-transaction
     (cl-loop for feed hash-values of feeds-hash
              for url = (elt feed 2)
              for feed-id = (ignore-errors (elfeed-legacy-lookup url))
              when feed-id do (elfeed-get-feed feed-id url))
     (cl-loop for entry hash-values of entries-hash
              for (old-id . feed-url) = (elt entry 1)
              for feed = (elfeed-get-feed-by-url feed-url)
              for feed-id = (if feed (elfeed-feed-id feed) "")
              for new-id = (if (string-match "^urn:uuid:" old-id)
                               old-id  ; atom id
                             (elfeed-uuid feed-id old-id))
              when feed do
              (elfeed-entry-save
               (elfeed-entry--create
                :id new-id
                :feed-id feed-id
                :title (elt entry 2)
                :link (elt entry 3)
                :tags (elt entry 8)
                :date (elt entry 4)
                :enclosures (elt entry 7)))))))

(provide 'elfeed-sql)

;;; elfeed-emacsql.el ends here
