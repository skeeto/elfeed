;;; elfeed-emacsql.el --- Emacsql database back-end -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-db)

(defvar elfeed-sql--connection nil)

(defvar elfeed-sql-schema-entries
  '([(entry-id integer :primary) feed-url link title (date float :non-nil)]
    :unique [feed-url link]))

(defvar elfeed-sql-schema-content
  '[(entry-id integer :primary :references (entries entry-id))
    content
    content-type])

(defvar elfeed-sql-schema-tags
  '([(entry-id :references (entries entry-id)) tag]
    :unique [entry-id tag]))

(defun elfeed-sql-db ()
  "Ensure that a database connection has been established and return it."
  (if elfeed-sql--connection
      elfeed-sql--connection
    (let ((db-file (expand-file-name "elfeed.sqlite" elfeed-db-directory)))
      (setf elfeed-sql--connection (emacsql-sqlite db-file :debug t)))))

(defun elfeed-sql-init ()
  "Initialize the Elfeed SQLite database."
  (emacsql-with-transaction (elfeed-sql-db)
    (cl-mapc (apply-partially
              #'emacsql (elfeed-sql-db) [:create-table (:if-not-exists $1) $2])
             '(entries content tags)
             (list elfeed-sql-schema-entries
                   elfeed-sql-schema-content
                   elfeed-sql-schema-tags))))

(defun elfeed-sql-drop ()
  "Drop all tables in the SQLite database."
  (emacsql-with-transaction (elfeed-sql-db)
    (mapc (apply-partially #'emacsql (elfeed-sql-db) [:drop-table $1])
          '(tags content entries))))

(defun elfeed-sql-entry-id (entry)
  "Find the surrogate ID for ENTRY, returning nil if not found."
  (caar
   (emacsql (elfeed-sql-db)
            [:select entry-id :from entries
             :where (and (= feed-url $1) (= link $2))]
            (elfeed-entry-feed-id entry)
            (elfeed-entry-link entry))))

(defun elfeed-sql-insert--entry (entry)
  "Insert or update ENTRY in the database."
  (emacsql-with-transaction (elfeed-sql-db)
    (let ((entry-id (elfeed-sql-entry-id entry)))
      (if entry-id
          (emacsql (elfeed-sql-db)
                   [:replace :into (entries [entry-id feed-url link title date])
                    :values $1]
                   (vector entry-id
                           (elfeed-entry-feed-id entry)
                           (elfeed-entry-link entry)
                           (elfeed-entry-title entry)
                           (elfeed-entry-date entry)))
        (emacsql (elfeed-sql-db)
                 [:insert :into (entries [feed-url link title date]) :values $1]
                 (vector (elfeed-entry-feed-id entry)
                           (elfeed-entry-link entry)
                           (elfeed-entry-title entry)
                           (elfeed-entry-date entry)))
        (setf entry-id (elfeed-sql-entry-id entry)))
      (emacsql-thread (elfeed-sql-db)
        ([:replace :into (content [entry-id content content-type]) :values $1]
         (vector entry-id
                 (elfeed-deref (elfeed-entry-content entry))
                 (elfeed-entry-content-type entry)))
        ([:delete :from tags :where (= entry-id $1)] entry-id))
      (let ((tags (mapcar (apply-partially #'vector entry-id)
                          (elfeed-entry-tags entry))))
        (when tags
          (emacsql (elfeed-sql-db)
                   [:replace :into (tags [entry-id tag]) :values $1] tags))))))

(defun elfeed-sql-insert (thing)
  "Insert or update THING in database."
  (cl-etypecase thing
    (elfeed-entry (elfeed-sql-insert--entry thing))))

(defun elfeed-sql-get-entry (feed-url link)
  "Return the entry for FEED-URL and LINK."
  (cl-flet ((q (sql &rest args) (apply #'emacsql (elfeed-sql-db) sql args)))
    (emacsql-with-bind (elfeed-sql-db)
        ([:select [e:entry-id feed-url link title date c:content c:content-type]
          :from [(as entries e) (as content c)]
          :where (and (= feed-url $1) (= link $2) (= e:entry-id c:entry-id))]
         feed-url link)
      (elfeed-entry--create
       :id (cons feed-url link)
       :feed-id feed-url
       :title title
       :link link
       :date date
       :content c:content
       :content-type c:content-type
       :tags (mapcar #'car (q [:select [tag] :from tags
                               :where (= entry-id $1)] e:entry-id))))))

(defun elfeed-sql-load-from-db ()
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
