;;; elfeed-db.el --- database and model for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This file exists *only* to provide the legacy interface to the new
;; database. New code shouldn't use anything from this file.

;;; Code:

(require 'cl-lib)
(require 'elfeed-lib)
(require 'elfeed-sql)

(defun elfeed-db-get-feed (url)
  "Get feed by URL, returning nil if it doesn't exist."
  (elfeed-get-feed-by-url url))
(make-obsolete 'elfeed-db-get-feed 'elfeed-get-feed-by-url "2.0.0")

(defun elfeed-db-get-entry (id)
  "Get an entry by its ID."
  (elfeed-get-entry id))
(make-obsolete 'elfeed-db-get-entry 'elfeed-get-entry "2.0.0")

(defun elfeed-db-add (entries)
  "Add ENTRIES to the database."
  (elfeed-with-transaction
    (mapc #'elfeed-entry-save entries))
  :success)
(make-obsolete 'elfeed-db-add 'elfeed-entry-save "2.0.0")

(defmacro with-elfeed-db-visit (entry-and-feed &rest body)
  "Visit each entry in the database from newest to oldest.
Use `elfeed-db-return' to exit early and optionally return data.

  (with-elfeed-db-visit (entry feed)
    (do-something entry)
    (when (some-date-criteria-p entry)
      (elfeed-db-return)))"
  (declare (indent defun))
  (let ((entry-var (cl-first entry-and-feed))
        (feed-var (cl-second entry-and-feed)))
    `(catch 'elfeed-db-done
       (cl-flet ((elfeed-db-return (&optional v) (throw 'elfeed-db-done v)))
         (let ((listing (elfeed-sql [:select [entry-id feed-id] :from entries
                                    :order-by [(desc entry-date)]])))
           (dolist (row listing)
             (let ((,entry-var (elfeed-get-entry (cl-first row)))
                   (,feed-var (elfeed-get-feed (cl-second row))))
               ,@body)))))))
(make-obsolete 'with-elfeed-db-visit 'elfeed-find "2.0.0")

;; Obsolete functions:

(defun elfeed-db-save ()
  "This function is a no-op."
  :success)
(make-obsolete 'elfeed-db-save nil "2.0.0")

(defun elfeed-deref (entry)
  "Return ENTRY (no-op)."
  entry)
(make-obsolete 'elfeed-deref nil "2.0.0")

(defun elfeed-db-gc (&optional _)
  "No-op."
  0)
(make-obsolete 'elfeed-db-gc 'elfeed-db-vacuum "2.0.0")

(defun elfeed-db-gc-safe ()
  "No-op.")
(make-obsolete 'elfeed-db-gc-safe 'elfeed-db-vacuum "2.0.0")

(defun elfeed-db-compact ()
  "No-op."
  t)
(make-obsolete 'elfeed-db-compact 'elfeed-db-vacuum "2.0.0")

(provide 'elfeed-db)

;;; elfeed-db.el ends here
