;;; elfeed-search.el --- list feed entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'wid-edit) ; widget-inactive face

(provide 'elfeed-search)

(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-lib)
(require 'elfeed-show)

(defvar elfeed-search-entries ()
  "List of the entries currently on display.")

(defvar elfeed-search-filter-history nil
  "Filter history for `completing-read'.")

(defvar elfeed-search-last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar elfeed-search-refresh-timer nil
  "The timer used to keep things updated as the database updates.")

(defcustom elfeed-search-filter "@6-months-ago +unread"
  "Query string filtering shown entries."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-sort-order 'descending
  "The order in which entries should be displayed, by time."
  :group 'elfeed
  :type '(choice (const descending) (const ascending)))

(defcustom elfeed-search-refresh-rate 3
  "How often the buffer should update against the datebase in seconds."
  :group 'elfeed
  :type 'number)

(defcustom elfeed-search-clipboard-type 'PRIMARY
  "Selects the clipboard `elfeed-search-yank' should use.
Choices are the symbols PRIMARY, SECONDARY, or CLIPBOARD."
  :group 'elfeed
  :type '(choice (const PRIMARY) (const SECONDARY) (const CLIPBOARD)))

(defvar elfeed-search-live nil
  "When true, Elfeed is currently reading a filter from the minibuffer.")

(defvar elfeed-search--offset 2
  "Offset between line numbers and entry list position.")

(defalias 'elfeed-search-tag-all-unread
  (elfeed-expose #'elfeed-search-tag-all 'unread)
  "Add the `unread' tag to all selected entries.")

(defalias 'elfeed-search-untag-all-unread
  (elfeed-expose #'elfeed-search-untag-all 'unread)
  "Remove the `unread' tag from all selected entries.")

(defalias 'elfeed-search-update--force
  (elfeed-expose #'elfeed-search-update :force)
  "Force refresh view of the feed listing.")

(defvar elfeed-search-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" 'quit-window)
      (define-key map "g" 'elfeed-search-update--force)
      (define-key map "G" 'elfeed-update)
      (define-key map (kbd "RET") 'elfeed-search-show-entry)
      (define-key map "s" 'elfeed-search-live-filter)
      (define-key map "S" 'elfeed-search-set-filter)
      (define-key map "b" 'elfeed-search-browse-url)
      (define-key map "y" 'elfeed-search-yank)
      (define-key map "u" 'elfeed-search-tag-all-unread)
      (define-key map "r" 'elfeed-search-untag-all-unread)
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)
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
  (buffer-disable-undo)
  (hl-line-mode)
  (make-local-variable 'elfeed-search-entries)
  (make-local-variable 'elfeed-search-filter)
  (when (null elfeed-search-refresh-timer)
    (setf elfeed-search-refresh-timer
          (run-at-time elfeed-search-refresh-rate elfeed-search-refresh-rate
                       #'elfeed-search-update)))
  (add-hook 'kill-buffer-hook #'elfeed-db-save t t)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (ignore-errors (cancel-timer elfeed-search-refresh-timer))
              (setf elfeed-search-refresh-timer nil))
            t t)
  (elfeed-search-update :force)
  (run-hooks 'elfeed-search-mode-hook))

(defun elfeed-search-buffer ()
  (get-buffer-create "*elfeed-search*"))

(defun elfeed-search-format-date (date)
  "Format a date for printing in elfeed-search-mode."
  (let ((string (format-time-string "%Y-%m-%d" (seconds-to-time date))))
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

(defcustom elfeed-search-title-max-width 70
  "Maximum column width for titles in the elfeed-search buffer."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-search-title-min-width 16
  "Minimum column width for titles in the elfeed-search buffer."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-search-trailing-width 30
  "Space reserved for displaying the feed and tag information."
  :group 'elfeed
  :type 'integer)

(defun elfeed-search-print (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces '(elfeed-search-title-face))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (when (elfeed-tagged-p 'unread entry)
      (push 'bold title-faces))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert "(" tags-str ")"))))

(defun elfeed-search-parse-filter (filter)
  "Parse the elements of a search filter."
  (let ((must-have ())
        (must-not-have ())
        (after nil)
        (matches ())
        (not-matches ()))
    (cl-loop for element in (split-string filter)
             for type = (aref element 0)
             do (cl-case type
                  (?+
                   (let ((symbol (intern (substring element 1))))
                     (unless (eq '## symbol)
                       (push symbol must-have))))
                  (?-
                   (let ((symbol (intern (substring element 1))))
                     (unless (eq '## symbol)
                       (push symbol must-not-have))))
                  (?@ (setf after (elfeed-time-duration (substring element 1))))
                  (?! (let ((re (substring element 1)))
                        (when (elfeed-valid-regexp-p re)
                          (push re not-matches))))
                  (otherwise (when (elfeed-valid-regexp-p element)
                               (push element matches)))))
    (list after must-have must-not-have matches not-matches)))

(defun elfeed-search-filter (filter entry feed)
  "Filter out only entries that match the filter. See
`elfeed-search-set-filter' for format/syntax documentation."
  (cl-destructuring-bind (after must-have must-not-have matches not-matches) filter
    (let* ((tags (elfeed-entry-tags entry))
           (date (elfeed-entry-date entry))
           (age (- (float-time) date))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry)))
           (link (elfeed-entry-link entry))
           (feed-title
            (or (elfeed-meta feed :title) (elfeed-feed-title feed) "")))
      (when (and after (> age after))
        (elfeed-db-return))
      (and (cl-every  (lambda (tag) (member tag tags)) must-have)
           (cl-notany (lambda (tag) (member tag tags)) must-not-have)
           (or (null matches)
               (cl-every
                (lambda (m)
                  (or (and title      (string-match-p m title))
                      (and link       (string-match-p m link))
                      (and feed-title (string-match-p m feed-title))))
                matches))
           (cl-notany (lambda (m)
                        (or (and title      (string-match-p m title))
                            (and link       (string-match-p m link))
                            (and feed-title (string-match-p m feed-title))))
                      not-matches)))))

(defun elfeed-search--prompt (current)
  "Prompt for a new filter, starting with CURRENT."
  (read-from-minibuffer
   "Filter: "
   (if (or (string= "" current)
           (string-match-p " $" current))
       current
     (concat current " "))
   nil nil 'elfeed-search-filter-history))

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
  (interactive (list (elfeed-search--prompt
                      (if current-prefix-arg "" elfeed-search-filter))))
  (with-current-buffer (elfeed-search-buffer)
    (setf elfeed-search-filter new-filter)
    (elfeed-search-update :force)))

(defun elfeed-search-insert-header-text (text)
  "Insert TEXT into buffer using header face."
  (insert (propertize text 'face '(widget-inactive italic))))

(defun elfeed-search-insert-intro-header ()
  "Insert the intro header with buttons."
  (cl-flet ((button (f)
              (insert-button (symbol-name f)
                             'follow-link t
                             'action (lambda (_) (call-interactively f)))))
    (elfeed-search-insert-header-text "Database empty. Use ")
    (button 'elfeed-add-feed)
    (elfeed-search-insert-header-text ", or ")
    (button 'elfeed-load-opml)
    (elfeed-search-insert-header-text ", or ")
    (button 'elfeed-update)
    (elfeed-search-insert-header-text ".")))

(defun elfeed-search-insert-header ()
  "Insert a one-line status header."
  (if (or elfeed-waiting elfeed-connections)
      (elfeed-search-insert-header-text
       (format "%d feeds pending, %d in process ..."
               (length elfeed-waiting) (length elfeed-connections)))
    (let ((time (seconds-to-time (elfeed-db-last-update))))
      (if (zerop (float-time time))
          (elfeed-search-insert-intro-header)
        (elfeed-search-insert-header-text
         (format "Database last updated %s"
                 (format-time-string "%A, %B %d %Y %H:%M:%S %Z" time)))))))

(defun elfeed-search--update-list ()
  "Update `elfeed-search-filter' list."
  (let* ((filter (elfeed-search-parse-filter elfeed-search-filter))
         (head (list nil))
         (tail head))
    (with-elfeed-db-visit (entry feed)
      (when (elfeed-search-filter filter entry feed)
        (setf (cdr tail) (list entry)
              tail (cdr tail))))
    (setf elfeed-search-entries
          (if (eq elfeed-sort-order 'ascending)
              (nreverse (cdr head))
            (cdr head)))))

(defmacro elfeed-save-excursion (&rest body)
  "Like `save-excursion', but by entry/line/column instead of point."
  (declare (indent defun))
  `(let ((entry (elfeed-search-selected :single))
         (line (line-number-at-pos))
         (column (current-column)))
     (unwind-protect
         (progn ,@body)
       (let ((entry-position (cl-position entry elfeed-search-entries)))
         (elfeed-goto-line (if entry-position
                               (+ elfeed-search--offset entry-position)
                             line))
         (move-to-column column)))))

(defun elfeed-search-update (&optional force)
  "Update the display to match the database."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (if (or force (and (not elfeed-search-live)
                       (< elfeed-search-last-update (elfeed-db-last-update))))
        (elfeed-save-excursion
          (let ((inhibit-read-only t)
                (standard-output (current-buffer)))
            (erase-buffer)
            (elfeed-search-insert-header)
            (insert "\n")
            (elfeed-search--update-list)
            (dolist (entry elfeed-search-entries)
              (elfeed-search-print entry)
              (insert "\n"))
            (insert "End of entries.\n")
            (setf elfeed-search-last-update (float-time))))
      (let ((inhibit-read-only t))
        (elfeed-save-excursion
          (goto-char (point-min))
          (elfeed-kill-line)
          (elfeed-search-insert-header))))))

(defun elfeed-search-update-line (&optional n)
  "Redraw the current line."
  (let ((inhibit-read-only t))
    (save-excursion
      (when n (elfeed-goto-line n))
      (let ((entry (elfeed-search-selected :ignore-region)))
        (when entry
          (elfeed-kill-line)
          (elfeed-search-print entry))))))

(defun elfeed-search-update-entry (entry)
  "Redraw a specific entry."
  (let ((n (cl-position entry elfeed-search-entries)))
    (when n (elfeed-search-update-line (+ elfeed-search--offset n)))))

(defun elfeed-search-selected (&optional ignore-region)
  "Return a list of the currently selected feeds."
  (let ((use-region (and (not ignore-region) (use-region-p))))
    (let ((start (if use-region (region-beginning) (point)))
          (end   (if use-region (region-end)       (point))))
      (cl-loop for line from (line-number-at-pos start)
               to (line-number-at-pos end)
               for offset = (- line elfeed-search--offset)
               when (and (>= offset 0) (nth offset elfeed-search-entries))
               collect it into selected
               finally (return (if ignore-region (car selected) selected))))))

(defun elfeed-search-browse-url ()
  "Visit the current entry in your browser using `browse-url'."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-search-yank ()
  "Copy the selected feed item to clipboard and kill-ring."
  (interactive)
  (let* ((entry (elfeed-search-selected :ignore-region))
         (link (and entry (elfeed-entry-link entry))))
    (when entry
      (elfeed-untag entry 'unread)
      (kill-new link)
      (x-set-selection elfeed-search-clipboard-type link)
      (message "Copied: %s" link)
      (elfeed-search-update-line)
      (forward-line))))

(defun elfeed-search-tag-all (tag)
  "Apply TAG to all selected entries."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-search-untag-all (tag)
  "Remove TAG from all selected entries."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-search-toggle-all (tag)
  "Toggle TAG on all selected entries."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             when (elfeed-tagged-p tag entry)
             do (elfeed-untag entry tag)
             else do (elfeed-tag entry tag))
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

;; Live Filters

(defun elfeed-search--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when elfeed-search-live
    (add-hook 'post-command-hook 'elfeed-search--live-update nil :local)))

(add-hook 'minibuffer-setup-hook 'elfeed-search--minibuffer-setup)

(defun elfeed-search--live-update ()
  "Update the elfeed-search buffer based on the contents of the minibuffer."
  (when elfeed-search-live
    (let ((buffer (elfeed-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let ((elfeed-search-filter current-filter))
            (elfeed-search-update :force)))))))

(defun elfeed-search-live-filter ()
  "Filter the elfeed-search buffer as the filter is written."
  (interactive)
  (unwind-protect
      (let ((elfeed-search-live t))
        (setq elfeed-search-filter
              (read-from-minibuffer "Filter: " elfeed-search-filter)))
    (elfeed-search-update :force)))

;;; elfeed-search.el ends here
