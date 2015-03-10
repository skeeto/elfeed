;;; elfeed-show.el --- display feed entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl-lib)
(require 'shr)
(require 'url-parse)
(require 'browse-url)
(require 'message) ; faces
(require 'elfeed-db)
(require 'elfeed-lib)
(require 'elfeed-search)

(defcustom elfeed-show-truncate-long-urls t
  "When non-nil, use an ellipsis to shorten very long displayed URLs."
  :group 'elfeed
  :type 'bool)

(defvar elfeed-show-entry nil
  "The entry being displayed in this buffer.")

(defvar elfeed-show-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" 'elfeed-kill-buffer)
      (define-key map "g" 'elfeed-show-refresh)
      (define-key map "n" 'elfeed-show-next)
      (define-key map "p" 'elfeed-show-prev)
      (define-key map "s" 'elfeed-show-new-live-search)
      (define-key map "b" 'elfeed-show-visit)
      (define-key map "y" 'elfeed-show-yank)
      (define-key map "u" (elfeed-expose #'elfeed-show-tag 'unread))
      (define-key map "+" 'elfeed-show-tag)
      (define-key map "-" 'elfeed-show-untag)
      (define-key map (kbd "SPC") 'scroll-up-command)
      (define-key map (kbd "DEL") 'scroll-down-command)
      (define-key map "\t" 'shr-next-link)
      (define-key map [tab] 'shr-next-link)
      (define-key map "\e\t" 'shr-previous-link)
      (define-key map [backtab] 'shr-previous-link)
      (define-key map [mouse-2] 'shr-browse-url)))
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
  (buffer-disable-undo)
  (make-local-variable 'elfeed-show-entry)
  (run-hooks 'elfeed-show-mode-hook))

(defun elfeed-insert-html (html &optional base-url)
  "Converted HTML markup to a propertized string."
  (shr-insert-document
   (if (elfeed-libxml-supported-p)
       (with-temp-buffer
         ;; insert <base> to work around libxml-parse-html-region bug
         (insert (format "<base href=\"%s\">" base-url))
         (insert html)
         (libxml-parse-html-region (point-min) (point-max) base-url))
     '(i () "Elfeed: libxml2 functionality is unavailable"))))

(cl-defun elfeed-insert-link (url &optional (content url))
  "Insert a clickable hyperlink to URL titled CONTENT."
  (when (and elfeed-show-truncate-long-urls
             (integerp shr-width)
             (> (length content) (- shr-width 8)))
    (let ((len (- (/ shr-width 2) 10)))
      (setq content (format "%s[...]%s"
                            (substring content 0 len)
                            (substring content (- len))))))
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
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (link (elfeed-entry-link elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tagsstr (mapconcat #'symbol-name tags ", "))
         (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (elfeed-feed-title feed))
         (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
    (erase-buffer)
    (insert (format (propertize "Title: %s\n" 'face 'message-header-name)
                    (propertize title 'face 'message-header-subject)))
    (insert (format (propertize "Date: %s\n" 'face 'message-header-name)
                    (propertize nicedate 'face 'message-header-other)))
    (insert (format (propertize "Feed: %s\n" 'face 'message-header-name)
                    (propertize feed-title 'face 'message-header-other)))
    (when tags
      (insert (format (propertize "Tags: %s\n" 'face 'message-header-name)
                      (propertize tagsstr 'face 'message-header-other))))
    (insert (propertize "Link: " 'face 'message-header-name))
    (elfeed-insert-link link link)
    (insert "\n")
    (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
             do (insert (propertize "Enclosure: " 'face 'message-header-name))
             do (elfeed-insert-link (car enclosure))
             do (insert "\n"))
    (insert "\n")
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
  (elfeed-kill-buffer)
  (with-current-buffer (elfeed-search-buffer)
    (forward-line -2)
    (call-interactively #'elfeed-search-show-entry)))

(defun elfeed-show-new-live-search ()
  "Kill the current buffer, search again in *elfeed-search*."
  (interactive)
  (elfeed-kill-buffer)
  (elfeed)
  (elfeed-search-live-filter))

(defun elfeed-show-visit ()
  "Visit the current entry in the browser."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (message "Sent to browser: %s" link)
      (browse-url link))))

(defun elfeed-show-yank ()
  "Copy the current entry link URL to the clipboard."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (kill-new link)
      (x-set-selection 'PRIMARY link)
      (message "Yanked: %s" link))))

(defun elfeed-show-tag (&rest tags)
  "Add TAGS to the displayed entry."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entry elfeed-show-entry))
    (apply #'elfeed-tag entry tags)
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry))
    (elfeed-show-refresh)))

(defun elfeed-show-untag (&rest tags)
  "Remove TAGS from the displayed entry."
  (interactive (let* ((tags (elfeed-entry-tags elfeed-show-entry))
                      (names (mapcar #'symbol-name tags))
                      (select (completing-read "Untag: " names nil :match)))
                 (list (intern select))))
  (let ((entry elfeed-show-entry))
    (apply #'elfeed-untag entry tags)
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry))
    (elfeed-show-refresh)))

(provide 'elfeed-show)

;;; elfeed-show.el ends here
