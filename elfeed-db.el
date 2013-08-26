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

(defvar elfeed-db (make-hash-table :test 'equal)
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
  (let ((feed (gethash url elfeed-db)))
    (or feed
        (setf (gethash url elfeed-db)
              (make-elfeed-feed
               :url url :entries (make-hash-table :test 'equal))))))

(defun elfeed-db-put (url entries)
  "Add entries to the database under URL."
  (let* ((feed (elfeed-db-get url))
         (table (elfeed-feed-entries feed)))
    (loop for entry in entries
          for id = (elfeed-entry-id entry)
          for old = (gethash id table)
          when old  ; merge old tags back in
          do (setf (elfeed-entry-tags entry) (elfeed-entry-tags old))
          when (not old)
          do (loop for hook in elfeed-new-entry-hook
                   do (funcall hook entry))
          do (setf (gethash id table) entry))
    (setf (gethash :last-update elfeed-db) (float-time))))

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
  (gethash :last-update elfeed-db 0))

(defun elfeed-sort (entries &optional old-first)
  "Destructively sort the given entries by date."
  (sort* entries (if old-first #'string< #'elfeed-string>)
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

(provide 'elfeed-db)

;;; elfeed-db.el ends here
