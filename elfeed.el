;;; elfeed.el --- an Emacs web feed reader -*- lexical-binding: t; -*-

;; Requires Emacs >= 24.

;;; Code:

(require 'cl)
(require 'xml)

(defgroup elfeed nil
  "An Emacs web feed reader."
  :group 'comm)

(defcustom elfeed-feeds ()
  "List of all feeds that elfeed should follow. You must add your
feeds to this list."
  :group 'elfeed
  :type 'list)

;; Model and Database

(defstruct elfeed-entry
  title id link date content)

(defvar elfeed-db (make-hash-table :test 'equal)
  "The core database for elfeed.")

(defun elfeed-db-get (feed)
  "Get/create the table for FEED."
  (let ((table (gethash feed elfeed-db)))
    (if (null table)
        (setf (gethash feed elfeed-db) (make-hash-table :test 'equal))
      table)))

(defun elfeed-db-put (feed entries)
  "Add entries to the database."
  (let ((table (elfeed-db-get feed)))
    (loop for entry in entries
          do (setf (gethash (elfeed-entry-id entry) table) entry))))

(defun elfeed-string> (a b)
  (string< b a))

(defun elfeed-sort (entries &optional old-first)
  "Destructively sort the given entries by date."
  (sort* entries (if old-first #'string< #'elfeed-string>)
         :key #'elfeed-entry-date))

(defun elfeed-db-entries (&optional feed)
  "Get all the entries for a feed, sorted by date."
  (elfeed-sort
   (if (null feed)
       (loop for feed being the hash-keys of elfeed-db
             append (elfeed-db-entries feed))
     (loop for entry being the hash-values of (elfeed-db-get feed)
           collect entry))))

;; Fetching:

(defcustom elfeed-max-connections 6
  "The maximum number of feeds to be fetched in parallel."
  :group 'elfeed
  :type 'integer)

(defvar elfeed-connections nil
  "List of callbacks awaiting responses.")

(defvar elfeed-waiting nil
  "List of requests awaiting connections.")

(defvar elfeed--connection-counter 0
  "Provides unique connection identifiers.")

(defun elfeed--check-queue ()
  "Start waiting connections if connection slots are available."
  (while (and elfeed-waiting
              (< (length elfeed-connections) elfeed-max-connections))
    (let ((request (pop elfeed-waiting)))
      (destructuring-bind (id url cb) request
        (push request elfeed-connections)
        (url-retrieve url cb)))))

(defun elfeed--wrap-callback (id cb)
  "Return a function that manages the elfeed queue."
  (lambda (status)
    (setf elfeed-connections (delete* id elfeed-connections :key #'car))
    (elfeed--check-queue)
    (funcall cb status)))

(defun elfeed-fetch (url callback)
  "Basically wraps `url-retrieve' but uses the connection limiter."
  (let* ((id (incf elfeed--connection-counter))
         (cb (elfeed--wrap-callback id callback)))
    (push (list id url cb) elfeed-waiting)
    (elfeed--check-queue)))

(defmacro with-elfeed-fetch (url &rest body)
  "Asynchronously run BODY in a buffer with the contents from
URL. This macro is anaphoric, with STATUS referring to the status
from `url-retrieve'."
  (declare (indent defun))
  `(elfeed-fetch ,url (lambda (status) ,@body (kill-buffer))))

;; Parsing:

(defun elfeed-feed-type (content)
  "Return the feed type given the parsed content (:atom, :rss) or
NIL for unknown."
  (cadr (assoc (caar content) '((feed :atom) (rss :rss)))))

(defun elfeed-rfc3339 (&optional time)
  "Return an RFC 3339 formatted date string (Atom style)."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (if time (date-to-time time) nil) t))

(defun elfeed-entries-from-atom (xml)
  "Turn parsed Atom content into a list of elfeed-entry structs."
  (loop for entry in (xml-query-all '(feed entry) xml) collect
        (let* ((title (or (xml-query '(title *) entry) ""))
               (anylink (xml-query '(link :href) entry))
               (altlink (xml-query '(link [rel "alternate"] :href) entry))
               (link (or altlink anylink))
               (id (or (xml-query '(id *) entry) link))
               (date (or (xml-query '(published *) entry)
                         (xml-query '(updated *) entry)
                         (xml-query '(date *) entry))))
          (make-elfeed-entry :title title :id id :link link
                             :date (elfeed-rfc3339 date) :content nil))))

(defun elfeed-entries-from-rss (xml)
  "Turn parsed RSS content into a list of elfeed-entry structs."
  (loop for item in (xml-query-all '(rss channel item) xml) collect
        (let* ((title (or (xml-query '(title *) item) ""))
               (link (xml-query '(link *) item))
               (guid (xml-query '(guid *) item))
               (id (or guid link))
               (date (or (xml-query '(pubDate *) item)
                         (xml-query '(date *) item))))
          (make-elfeed-entry :title title :id id :link link
                             :date (elfeed-rfc3339 date) :content nil))))

(defun elfeed-update-feed (url)
  "Update a specific feed."
  (interactive (list (completing-read "Feed: " elfeed-feeds)))
  (with-elfeed-fetch url
    (unless status
      (goto-char (point-min))
      (search-forward "\n\n") ; skip HTTP headers
      (set-buffer-multibyte t)
      (let* ((xml (xml-parse-region (point) (point-max)))
             (entries (case (elfeed-feed-type xml)
                        (:atom (elfeed-entries-from-atom xml))
                        (:rss (elfeed-entries-from-rss xml))
                        (t (error "Unkown feed type.")))))
        (elfeed-db-put url entries)))))

(defun elfeed-update ()
  "Update all the feeds in `elfeed-feeds'."
  (mapc #'elfeed-update-feed elfeed-feeds))

;; Interface:

(defvar elfeed-search-entries ()
  "List of the entries currently on display.")

(defvar elfeed-search-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "q" 'quit-window)
      (define-key map "g" 'elfeed-search-update)
      (define-key map "b" 'elfeed-search-browse-url)))
  "Keymap for elfeed-search-mode.")

(defun elfeed-search-mode ()
  "Major mode for listing elfeed feed entries."
  (interactive)
  (kill-all-local-variables)
  (use-local-map elfeed-search-mode-map)
  (setq major-mode 'elfeed-search-mode
        mode-name "elfeed-search"
        truncate-lines t
        buffer-read-only t)
  (make-local-variable 'elfeed-search-entries)
  (elfeed-search-update))

(defun elfeed-buffer ()
  (get-buffer-create "*elfeed-search*"))

(defun elfeed ()
  "Enter elfeed."
  (interactive)
  (switch-to-buffer (elfeed-buffer))
  (unless (eq major-mode 'elfeed-search-mode) (elfeed-search-mode))
  (elfeed-search-update))

(defun elfeed-search-format-date (date)
  "Format a date for printing in elfeed-search-mode."
  (let ((string (format-time-string "%Y-%m-%d" (date-to-time date))))
    (format "%-10.10s" string)))

(defface elfeed-search-date-face
  '((((class color) (background light)) (:foreground "#333"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used in search mode for dates."
  :group 'elfeed)

(defface elfeed-search-title-face
  '((((class color) (background light)) (:foreground "#000"))
    (((class color) (background dark))  (:foreground "#fff")))
  "Face used in search mode for titles."
  :group 'elfeed)

(defun elfeed-search-update ()
  "Update the display to match the database."
  (interactive)
  (with-current-buffer (elfeed-buffer)
    (let ((inhibit-read-only t)
          (standard-output (current-buffer))
          (line (line-number-at-pos)))
      (erase-buffer)
      (loop for entry in (setf elfeed-search-entries (elfeed-db-entries))
            for date = (elfeed-search-format-date (elfeed-entry-date entry))
            for title = (remove ?\n (elfeed-entry-title entry))
            do (insert (propertize date 'face 'elfeed-search-date-face) " ")
            do (insert (propertize title 'face 'elfeed-search-title-face) "\n"))
      (insert "End of entries.\n")
      (goto-line line))))

(defun elfeed-search-selected ()
  "Return a list of the currently selected feeds."
  (let ((start (if (use-region-p) (region-beginning) (point)))
        (end   (if (use-region-p) (region-end) (point))))
    (loop for line from (line-number-at-pos start) to (line-number-at-pos end)
          when (nth (1- line) elfeed-search-entries)
          collect it)))

(defun elfeed-search-browse-url ()
  "Visit the current entry in your browser using `browse-url'."
  (interactive)
  (loop for entry in (elfeed-search-selected)
        when (elfeed-entry-link entry)
        do (browse-url it)))

(provide 'elfeed)

;;; elfeed.el ends here
