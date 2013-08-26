;;; elfeed.el --- an Emacs web feed reader -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Elfreed is a web feed client for Emacs, inspired by notmuch. See
;; the README for full documentation.

;; Elfreed requires Emacs 24 (lexical closures).

;;; Code:

(require 'cl)
(require 'shr)
(require 'xml)
(require 'xml-query)
(require 'url-parse)

(defgroup elfeed nil
  "An Emacs web feed reader."
  :group 'comm)

(defcustom elfeed-feeds ()
  "List of all feeds that elfeed should follow. You must add your
feeds to this list."
  :group 'elfeed
  :type 'list)

;; Model and Database

(defvar elfeed-db (make-hash-table :test 'equal)
  "The core database for elfeed.")

(defvar elfeed-initial-tags '(unread)
  "Initial tags for new entries.")

(defvar elfeed-new-entry-hook ()
  "Functions in this list are called with the new entry as its
argument. This is a chance to add cutoms tags to new entries.")

(defstruct elfeed-feed
  "A web feed, contains elfeed-entry structs."
  title url entries)

(defstruct elfeed-entry
  "A single entry from a feed, normalized towards Atom."
  title id link date content content-type tags feed)

(defun elfeed-tag (entry &rest tags)
  "Add a tag to an entry."
  (let ((current (elfeed-entry-tags entry)))
    (setf (elfeed-entry-tags entry) (remove-duplicates (append tags current)))))

(defun elfeed-untag (entry &rest tags)
  "Remove tags from an entry."
  (setf (elfeed-entry-tags entry)
        (loop for tag in (elfeed-entry-tags entry)
              unless (member tag tags) collect tag)))

(defun elfeed-tagged-p (tag entry)
  "Return true if ENTRY is tagged by TAG."
  (member tag (elfeed-entry-tags entry)))

(defun elfeed-db-get (url)
  "Get/create the table for URL."
  (let ((feed (gethash url elfeed-db)))
    (if feed
        feed
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

(defun elfeed-db-last-update ()
  "Return the last database update time in (`float-time') seconds."
  (gethash :last-update elfeed-db 0))

(defun elfeed-string> (a b)
  (string< b a))

(defun elfeed-sort (entries &optional old-first)
  "Destructively sort the given entries by date."
  (sort* entries (if old-first #'string< #'elfeed-string>)
         :key #'elfeed-entry-date))

(defun elfeed-db-entries (&optional url)
  "Get all the entries for a feed, sorted by date."
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
      (destructuring-bind (_ url cb) request
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
                      (if time (ignore-errors (date-to-time time)) nil) t))

(defun elfeed-cleanup (name)
  "Cleanup things that will be printed."
  (let ((trim (replace-regexp-in-string "^ +\\| +$" "" name)))
    (replace-regexp-in-string "[\n\t]+" " " trim)))

(defun elfeed-entries-from-atom (url xml)
  "Turn parsed Atom content into a list of elfeed-entry structs."
  (let ((feed (elfeed-db-get url))
        (title (xml-query '(feed title *) xml)))
    (setf (elfeed-feed-title feed) title)
    (loop for entry in (xml-query-all '(feed entry) xml) collect
          (let* ((title (or (xml-query '(title *) entry) ""))
                 (anylink (xml-query '(link :href) entry))
                 (altlink (xml-query '(link [rel "alternate"] :href) entry))
                 (link (or altlink anylink))
                 (id (or (xml-query '(id *) entry) link))
                 (date (or (xml-query '(published *) entry)
                           (xml-query '(updated *) entry)
                           (xml-query '(date *) entry)))
                 (content (xml-query '(content *) entry))
                 (type (or (xml-query '(content :type) entry) "")))
            (make-elfeed-entry :title (elfeed-cleanup title) :feed feed
                               :id (elfeed-cleanup id) :link link
                               :tags (copy-seq elfeed-initial-tags)
                               :date (elfeed-rfc3339 date) :content content
                               :content-type (if (string-match-p "html" type)
                                                 'html
                                               nil))))))

(defun elfeed-entries-from-rss (url xml)
  "Turn parsed RSS content into a list of elfeed-entry structs."
  (let ((feed (elfeed-db-get url))
        (title (xml-query '(rss channel title *) xml)))
    (setf (elfeed-feed-title feed) title)
    (loop for item in (xml-query-all '(rss channel item) xml) collect
          (let* ((title (or (xml-query '(title *) item) ""))
                 (link (xml-query '(link *) item))
                 (guid (xml-query '(guid *) item))
                 (id (or guid link))
                 (date (or (xml-query '(pubDate *) item)
                           (xml-query '(date *) item)))
                 (description (xml-query '(description *) item)))
            (make-elfeed-entry :title (elfeed-cleanup title)
                               :id (elfeed-cleanup id) :feed feed :link link
                               :tags (copy-seq elfeed-initial-tags)
                               :date (elfeed-rfc3339 date)
                               :content description :content-type 'html)))))

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
                        (:atom (elfeed-entries-from-atom url xml))
                        (:rss (elfeed-entries-from-rss url xml))
                        (t (error "Unkown feed type.")))))
        (elfeed-db-put url entries)))))

(defun elfeed-add-feed (url)
  "Manually add a feed to the database."
  (interactive (list (read-from-minibuffer "URL: " (x-get-selection-value))))
  (pushnew url elfeed-feeds :test #'string=)
  (elfeed-update-feed url))

(defun elfeed-update ()
  "Update all the feeds in `elfeed-feeds'."
  (interactive)
  (mapc #'elfeed-update-feed elfeed-feeds))

;; Interface:

(defvar elfeed-search-entries ()
  "List of the entries currently on display.")

(defvar elfeed-search-filter "+unread"
  "Query string filtering shown entires.")

(defvar elfeed-search-last-update 0
  "The last time the buffer was redrawn.")

(defvar elfeed-search-refresh-timer nil
  "The timer used to keep things updated as the database updates.")

(defcustom elfeed-search-refresh-rate 5
  "How often the buffer should update against the datebase in seconds.")

(defun elfeed-expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda () (interactive) (apply function args)))

(defvar elfeed-search-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "q" 'quit-window)
      (define-key map "g" (elfeed-expose #'elfeed-search-update :force))
      (define-key map "G" 'elfeed-update)
      (define-key map (kbd "RET") 'elfeed-search-show-entry)
      (define-key map "s" 'elfeed-search-set-filter)
      (define-key map "b" 'elfeed-search-browse-url)
      (define-key map "y" 'elfeed-search-yank)
      (define-key map "u" (elfeed-expose #'elfeed-search-tag-all 'unread))
      (define-key map "r" (elfeed-expose #'elfeed-search-untag-all 'unread))
      (define-key map "+" 'elfeed-search-tag-all)
      (define-key map "-" 'elfeed-search-untag-all)))
  "Keymap for elfeed-search-mode.")

(defun elfeed-search-mode ()
  "Major mode for listing elfeed feed entries.
\\{elfeed-search-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map elfeed-search-mode-map)
  (setq major-mode 'elfeed-search-mode
        mode-name "elfeed-search"
        truncate-lines t
        buffer-read-only t)
  (hl-line-mode)
  (make-local-variable 'elfeed-search-entries)
  (make-local-variable 'elfeed-search-filter)
  (when (null elfeed-search-refresh-timer)
    (setf elfeed-search-refresh-timer
          (run-at-time elfeed-search-refresh-rate elfeed-search-refresh-rate
                       (apply-partially #'elfeed-search-update))))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (message "killed timer?")
              (ignore-errors (cancel-timer elfeed-search-refresh-timer))
              (setf elfeed-search-refresh-timer nil))
            t t)
  (elfeed-search-update :force)
  (run-hooks 'elfeed-search-mode-hook))

(defun elfeed-search-buffer ()
  (get-buffer-create "*elfeed-search*"))

(defun elfeed ()
  "Enter elfeed."
  (interactive)
  (switch-to-buffer (elfeed-search-buffer))
  (unless (eq major-mode 'elfeed-search-mode)
    (elfeed-search-mode))
  (elfeed-search-update))

(defun elfeed-search-format-date (date)
  "Format a date for printing in elfeed-search-mode."
  (let ((string (format-time-string "%Y-%m-%d" (date-to-time date))))
    (format "%-10.10s" string)))

(defface elfeed-search-date-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used in search mode for dates."
  :group 'elfeed)

(defface elfeed-search-title-face
  '((((class color) (background light)) (:foreground "#000"))
    (((class color) (background dark))  (:foreground "#fff")))
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-feed-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used in search mode for feed titles."
  :group 'elfeed)

(defface elfeed-search-tag-face
  '((((class color) (background light)) (:foreground "#070"))
    (((class color) (background dark))  (:foreground "#0f0")))
  "Face used in search mode for tags."
  :group 'elfeed)

(defun elfeed-search-print (entry)
  "Print a single entry to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (elfeed-entry-title entry))
         (title-faces '(elfeed-search-title-face))
         (feed (elfeed-entry-feed entry))
         (feedtitle (if feed (elfeed-feed-title feed)))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ",")))
    (when (elfeed-tagged-p 'unread entry)
      (push 'bold title-faces))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title 'face title-faces) " ")
    (when feedtitle
      (insert "(" (propertize feedtitle 'face 'elfeed-search-feed-face) ") "))
    (when tags
      (insert "(" tags-str ")"))))

(defun elfeed-search-filter (entries)
  "Filter out only entries that match the filter. See
`elfeed-search-set-filter' for format/syntax documentation."
  (let ((must-have ())
        (must-not-have ())
        (after nil)
        (matches ()))
    (loop for element in (split-string elfeed-search-filter)
          for type = (aref element 0)
          do (case type
               (?+ (push (intern (substring element 1)) must-have))
               (?- (push (intern (substring element 1)) must-not-have))
               (?@ (setf after (elfeed-time-duration (substring element 1))))
               (t  (push element matches))))
    (loop for entry in entries
          for tags = (elfeed-entry-tags entry)
          for date = (float-time (date-to-time (elfeed-entry-date entry)))
          for age = (- (float-time) date)
          for title = (elfeed-entry-title entry)
          for link = (elfeed-entry-link entry)
          for feed = (elfeed-entry-feed entry)
          for feed-title = (or (elfeed-feed-title feed) "")
          when (and (every  (lambda (tag) (member tag tags)) must-have)
                    (notany (lambda (tag) (member tag tags)) must-not-have)
                    (or (not after)
                        (< age after))
                    (or (null matches)
                        (some (lambda (m) (or (string-match-p m title)
                                              (string-match-p m link)
                                              (string-match-p m feed-title)))
                              matches)))
          collect entry)))

(defun elfeed-search-set-filter (new-filter)
  "Set a new search filter for the elfeed-search buffer.

When given a prefix argument, the current filter is not displayed
in the minibuffer when prompting for a new filter.

Any component beginning with a + or - is treated as a tag. If +
the tag must be present on the entry. If - the tag must *not* be
present on the entry. Ex. \"+unread\" or \"+unread -comic\".

Any component beginning with an @ is an age limit. No posts older
than this are allowed. Ex. \"@3-days-ago\" or \"@1-year-old\".

Every other space-seperated element is treated like a regular
expression, matching against entry link, title, and feed title."
  (interactive (list (read-from-minibuffer
                      "Filter: " (if current-prefix-arg
                                     ""
                                   (concat elfeed-search-filter " ")))))
  (with-current-buffer (elfeed-search-buffer)
    (setf elfeed-search-filter new-filter)
    (elfeed-search-update :force)))

(defun elfeed-goto-line (n)
  "Like `goto-line' but for non-interactive use."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun elfeed-search-update (&optional force)
  "Update the display to match the database."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (when (or force (< elfeed-search-last-update (elfeed-db-last-update)))
      (let ((inhibit-read-only t)
            (standard-output (current-buffer))
            (line (line-number-at-pos)))
        (erase-buffer)
        (setf elfeed-search-entries (elfeed-search-filter (elfeed-db-entries)))
        (loop for entry in elfeed-search-entries
              do (elfeed-search-print entry)
              do (insert "\n"))
        (insert "End of entries.\n")
        (elfeed-goto-line line))
      (setf elfeed-search-last-update (float-time)))))

(defun elfeed-search-update-line (&optional n)
  "Redraw the current line."
  (let ((inhibit-read-only t))
    (save-excursion
      (when n (elfeed-goto-line n))
      (let ((entry (elfeed-search-selected :ignore-region)))
        (when entry
          (beginning-of-line)
          (let ((start (point)))
            (end-of-line)
            (delete-region start (point)))
          (elfeed-search-print entry))))))

(defun elfeed-search-update-entry (entry)
  "Redraw a specific entry."
  (let ((n (position entry elfeed-search-entries)))
    (when n (elfeed-search-update-line (1+ n)))))

(defun elfeed-search-selected (&optional ignore-region)
  "Return a list of the currently selected feeds."
  (let ((use-region (and (not ignore-region) (use-region-p))))
    (let ((start (if use-region (region-beginning) (point)))
          (end   (if use-region (region-end)       (point))))
      (loop for line from (line-number-at-pos start) to (line-number-at-pos end)
            when (nth (1- line) elfeed-search-entries)
            collect it into selected
            finally (return (if ignore-region (car selected) selected))))))

(defun elfeed-search-browse-url ()
  "Visit the current entry in your browser using `browse-url'."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (loop for entry in entries
          do (elfeed-untag entry 'unread)
          when (elfeed-entry-link entry)
          do (browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-search-yank ()
  "Copy the selected feed item to "
  (interactive)
  (let* ((entry (elfeed-search-selected :ignore-region))
         (link (and entry (elfeed-entry-link entry))))
    (when entry
      (elfeed-untag entry 'unread)
      (x-set-selection 'PRIMARY link)
      (message "Copied: %s" link)
      (elfeed-search-update-line)
      (forward-line))))

(defun elfeed-search-tag-all (tag)
  "Apply TAG to all selected entries."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entries (elfeed-search-selected)))
    (loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-search-untag-all (tag)
  "Remove TAG from all selected entries."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entries (elfeed-search-selected)))
    (loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-search-show-entry (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (forward-line)
    (elfeed-show-entry entry)))

;; Helper functions

(defun elfeed-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun elfeed-regexp-tagger (regexp tag)
  "Return a function suitable for `elfeed-new-entry-hook' that
tags entries with title or link matching regexp."
  (lambda (entry)
    (when (or (string-match-p regexp (elfeed-entry-link entry))
              (string-match-p regexp (elfeed-entry-title entry)))
      (elfeed-tag entry tag))))

(defun elfeed-time-duration (time)
  "Turn a time expression into a number of seconds. Uses
`timer-duration' but allows a bit more flair."
  (if (numberp time)
      time
    (timer-duration (replace-regexp-in-string "\\(ago\\|old\\|-\\)" "" time))))

(defun elfeed-time-untagger (time tag)
  "Return a function suitable for `elfeed-new-entry-hook' that
untags entries older than TIME. Uses `timer-duration' to parse
TIME, so relative strings are allowed. You probably want to use
this to remove 'unread' from older entries (database
initialization).

 (add-hook 'elfeed-new-entry-hook
           (elfeed-time-untagger \"2 weeks ago\" 'unread))"
  (let ((secs (elfeed-time-duration time)))
    (lambda (entry)
      (let ((time (float-time (date-to-time (elfeed-entry-date entry)))))
        (when (< time (- (float-time) secs))
          (elfeed-untag entry tag)
          :untag)))))

;; Entry display

(defvar elfeed-show-entry nil
  "The current entry being displayed.")

(defvar elfeed-show-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "q" 'elfeed-kill-buffer)
      (define-key map "g" 'elfeed-show-refresh)
      (define-key map "n" 'elfeed-show-next)
      (define-key map "p" 'elfeed-show-prev)
      (define-key map "b" 'elfeed-show-visit)
      (define-key map "y" 'elfeed-show-yank)
      (define-key map "u" (expose #'elfeed-show-tag 'unread))))
  "Keymap for `elfeed-show-mode'.")

(defun elfeed-show-mode ()
  "Mode for displaying Elfeed feed entries.
\\{elfeed-show-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map elfeed-show-mode-map)
  (setq major-mode 'elfeed-show-mode
        mode-name "elfeed-show"
        buffer-read-only t)
  (make-local-variable 'elfeed-show-entry)
  (run-hooks 'elfeed-show-mode-hook))

(defun elfeed-insert-html (html &optional base-url)
  "Converted HTML markup to a propertized string."
  (shr-insert-document
   (with-temp-buffer
     ;; insert <base> to work around libxml-parse-html-region bug
     (insert (format "<base href=\"%s\">" base-url))
     (insert html)
     (libxml-parse-html-region (point-min) (point-max) base-url))))

(defun* elfeed-insert-link (url &optional (content url))
  "Insert a clickable hyperlink to URL titled CONTENT."
  (elfeed-insert-html (format "<a href=\"%s\">%s</a>" url content)))

(defun elfeed-compute-base (url)
  "Return the base URL for URL, useful for relative paths."
  (let ((obj (url-generic-parse-url url)))
    (setf (url-filename obj) nil)
    (setf (url-target obj) nil)
    (url-recreate-url obj)))

(defun elfeed-show-refresh ()
  "Update the buffer to match the selected entry."
  (interactive)
  (let* ((inhibit-read-only t)
         (title (elfeed-entry-title elfeed-show-entry))
         (date (date-to-time (elfeed-entry-date elfeed-show-entry)))
         (link (elfeed-entry-link elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tagsstr (mapconcat #'symbol-name tags ", "))
         (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
         (content (elfeed-entry-content elfeed-show-entry))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
    (erase-buffer)
    (insert (format (propertize "Title: %s\n" 'face 'message-header-name)
                    (propertize title 'face 'message-header-subject)))
    (insert (format (propertize "Date: %s\n" 'face 'message-header-name)
                    (propertize nicedate 'face 'message-header-other)))
    (when tags
      (insert (format (propertize "Tags: %s\n" 'face 'message-header-name)
                      (propertize tagsstr 'face 'message-header-other))))
    (insert (propertize "Link: " 'face 'message-header-name))
    (elfeed-insert-link link link)
    (insert "\n\n")
    (if content
        (if (eq type 'html)
            (elfeed-insert-html content base)
          (insert content))
      (insert (propertize "(empty)\n" 'face 'italic)))
    (goto-char (point-min))))

(defun elfeed-show-entry (entry)
  "Display ENTRY in the current buffer."
  (let ((title (elfeed-entry-title entry)))
    (switch-to-buffer (get-buffer-create (format "*elfeed %s*" title)))
    (unless (eq major-mode 'elfeed-show-mode)
      (elfeed-show-mode))
    (setq elfeed-show-entry entry)
    (elfeed-show-refresh)))

(defun elfeed-show-next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (elfeed-kill-buffer)
  (with-current-buffer (elfeed-search-buffer)
    (call-interactively #'elfeed-search-show-entry)))

(defun elfeed-show-prev ()
  "Show the previous item in the elfeed-search buffer."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (forward-line -2)
    (call-interactively #'elfeed-search-show-entry)))

(defun elfeed-show-visit ()
  "Visit the current entry in the browser."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link (browse-url link))))

(defun elfeed-show-yank ()
  "Visit the current entry in the browser."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (x-set-selection 'PRIMARY link)
      (message "Yanked: %s" link))))

(defun elfeed-show-tag (&rest tags)
  "Add TAGS to the displayed entry."
  (let ((entry elfeed-show-entry))
    (apply #'elfeed-tag entry tags)
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry))))

(provide 'elfeed)

;;; elfeed.el ends here
