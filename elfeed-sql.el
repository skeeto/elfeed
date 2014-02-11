;;; elfeed-emacsql.el --- Emacsql database back-end -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'emacsql)
(require 'elfeed-db)

(defcustom elfeed-db-directory "~/.elfeed"
  "Directory where elfeed will store its database."
  :group 'elfeed
  :type 'directory)

(cl-defstruct (elfeed-thing (:constructor elfeed-thing--create))
  "Parent struct for all Elfeed structs."
  meta)

(cl-defstruct (elfeed-feed (:constructor elfeed-feed--create)
                           (:include elfeed-thing))
  "A web feed, contains elfeed-entry structs."
  id link title author)

(cl-defstruct (elfeed-entry (:constructor elfeed-entry--create)
                            (:include elfeed-thing))
  "A single entry from a feed, normalized towards Atom."
  id feed-id link title date -content -content-type enclosures tags)

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
  '([(entry-id integer :primary-key)
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

(defvar elfeed-sql--connection nil
  "Stores the EmacSQL database connection.")

(defun elfeed-sql-db ()
  "Ensure that a database connection has been established and return it."
  (if (and elfeed-sql--connection (emacsql-live-p elfeed-sql--connection))
      elfeed-sql--connection
    (let ((db-file (expand-file-name "elfeed.sqlite" elfeed-db-directory)))
      (prog1 (setf elfeed-sql--connection (emacsql-sqlite db-file :debug t))
        (elfeed-sql-init)))))

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

(defun elfeed-sql-drop ()
  "Drop all tables in the SQLite database."
  (emacsql-with-transaction (elfeed-sql-db)
    (mapc (apply-partially #'emacsql (elfeed-sql-db) [:drop-table $i1])
          '(enclosures tags content entries feeds))))

(defun elfeed-uuid (&rest strings)
  "Create a UUID suitable for a feed-id or entry-id from STRINGS."
  (let ((hash (secure-hash 'sha1 (mapconcat #'identity strings "!"))))
    (format "urn:uuid:%s-4%s-%s-%s-%s"
            (substring hash 0 8)
            (substring hash 8 11)
            (substring hash 11 15)
            (substring hash 15 19)
            (substring hash 19 31))))

(defun elfeed-get-feed (feed-id)
  "Get a feed by its FEED-ID."
  (let ((result (emacsql (elfeed-sql-db)
                         [:select [feed-id feed-link feed-title feed-author]
                          :from feeds :where (= feed-id $s1)]
                         feed-id)))
    (when result
      (vconcat (list 'cl-struct-elfeed-feed nil) (car result)))))

(defun elfeed-get-entry-tags (entry-id)
  (mapcar #'car (emacsql (elfeed-sql-db)
                         [:select [tag] :from tags
                          :where (= entry-id $s1)]
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

(defun elfeed-fill-entry-content (entry)
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
  (elfeed-fill-entry-content entry)
  (elfeed-entry--content entry))

(defun elfeed-entry-content-type (entry)
  "Get the content-type for ENTRY lazily."
  (elfeed-fill-entry-content entry)
  (elfeed-entry--content-type entry))

(defun elfeed-feed-save (feed)
  "Update FEED in the database."
  (emacsql (elfeed-sql-db)
           [:replace-into feeds [feed-id feed-link feed-title feed-author]
            :values $v1]
           (subseq feed 2 6)))

(defun elfeed-entry-save (entry)
  "Insert or update ENTRY in the database."
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
                      entry-id))))))

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

(defun elfeed-sql-load-old-db ()
  "Load everything from the old Elfeed database."
  (let ((entries nil))
    (with-elfeed-db-visit (e _)
      (push (copy-sequence e) entries))
    (dolist (e entries)
      (setf (elfeed-entry-content e) (elfeed-deref (elfeed-entry-content e))))
    (emacsql-with-transaction (elfeed-sql-db)
      (dolist (e entries)
        (elfeed-sql-insert--entry e)))))

(provide 'elfeed-emacsql)

;;; elfeed-emacsql.el ends here
