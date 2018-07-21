;;; elfeed-search.el --- list feed entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;
;; This is a sketch of a summary mode for elfeed, based on a conversation here:
;; https://www.reddit.com/r/emacs/comments/8y9ikh/elfeed_summarymetasearch_view
;;
;; TODO:
;;   - desktop support?
;;   - hooks?
;;   - bookmarks support?
;;   - user might want to configure columns
;;

;;; Code:

(require 'elfeed-search)

(defcustom elfeed-summary-sort-key "TITLE"
  "Sort the summary by TOTAL, UNREAD, DATE, or TITLE."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-summary-line-format "%10s %-70s %s/%s"
  "Controls summary columns: date, title, unread/total"
  :group 'elfeed
  :type 'string)

(defcustom elfeed-summary-date-format '("%Y-%m-%d" 10 :left)
  "The `format-time-string' format, target width, and alignment for dates.

This should be (string integer keyword) for (format width alignment).
Possible alignments are :left and :right."
  :group 'elfeed
  :type '(list string integer (choice (const :left) (const :right))))

(defface elfeed-summary-date-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used in summary mode for dates."
  :group 'elfeed)

(defface elfeed-summary-feed-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used in summary mode for feed titles."
  :group 'elfeed)

(defface elfeed-summary-unread-counts-face
  '((((class color) (background light)) (:foreground "#070"))
    (((class color) (background dark))  (:foreground "#0f0")))
  "Face used in summary mode for unread count."
  :group 'elfeed)

;; steal from siblings
(defalias 'elfeed-summary-format-date #'elfeed-search-format-date)

(defun elfeed-gather-summary ()
  (let ((table (make-hash-table :test 'eq)))
    ;; Stuff all the feeds we care about in a table
    ;; Each entry is (total-count unread-count most-recent-entry)
    (dolist (url (elfeed-feed-list))
      (setf (gethash (elfeed-db-get-feed url) table)
            (list 0 0 nil)))

    ;; Visit every database entry efficiently ordered by time, descending
    (with-elfeed-db-visit (entry feed)
      (let ((info (gethash feed table)))
        (when info
          (when (= 1 (cl-incf (car info)))
            (setf (caddr info) entry))
          (when (memq 'unread (elfeed-entry-tags entry))
            (cl-incf (cadr info))))))

    ;; Create a table of the results
    (cl-loop for feed being the hash-keys of table
             using (hash-values info)
             for (total unread most-recent) = info
                 for title = (or (elfeed-feed-title feed) "<no-title>")
		 for date = (when most-recent
			      (elfeed-summary-format-date (elfeed-entry-date most-recent)))
	         if (> unread 0)
		 collect (list
			  :feedid (elfeed-feed-id feed)
			  :title title
                          :total total
                          :unread unread
                          :date date)
		 into items
		 finally return (elfeed-summary-sort! items))))

(defun elfeed-summary-sort! (items)
   (cl-sort items
	    (cond ((equal elfeed-summary-sort-key "DATE") #'string<)
		  ((equal elfeed-summary-sort-key "TOTAL") #'<)
		  ((equal elfeed-summary-sort-key "UNREAD") #'<)
		  ((equal elfeed-summary-sort-key "TITLE") #'string<)
		  (t (error "elfeed-summary-sort-key must be DATE, TOTAL, UNREAD, or TITLE")))
	    :key (lambda (x) (plist-get x elfeed-summary-sort-key))))

(defun elfeed-summary-quit-window ()
  "Close the elfeed summary window."
  (interactive)
  (quit-window))

(defun elfeed-summary-selected-feed ()
  "Locate the feedid hidden in the text property of the current line"
  (plist-get (text-properties-at (point)) 'feedid))

(defun elfeed-summary-show-entry ()
  "Display the currently selected item in a buffer."
  (interactive)
  (let ((feedid (elfeed-summary-selected-feed)))
    (when feedid
      (elfeed-enter (elfeed-search-buffer) 'elfeed-search-mode)
      (elfeed-search-set-filter (concat "+unread =" feedid)))))

(defvar elfeed-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" 'elfeed-summary-quit-window)
      (define-key map (kbd "RET") 'elfeed-summary-show-entry)
      (define-key map "g" 'elfeed-refresh-feeds)
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)
      ;; TODO: command to force update all feeds
      ;; TODO: command to force update current feed
      ;; TODO: command to mark all of current feed up to date
      ;;
  "Keymap for elfeed-summary-mode.")))

(defun elfeed-summary-buffer ()
  (get-buffer-create "*elfeed-summary*"))

(defun elfeed-summary-update ()
  "Recompute feed summaries"
  (interactive)
  (with-current-buffer (elfeed-summary-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (item (elfeed-gather-summary))
	(let ((line (format elfeed-summary-line-format
			    (propertize (plist-get item :date) 'face 'elfeed-summary-date-face)
			    (propertize (plist-get item :title) 'face 'elfeed-summary-feed-face)
			    (propertize (int-to-string (plist-get item  :unread)) 'face 'elfeed-summary-unread-counts-face)
			    (propertize (int-to-string (plist-get item :total)) 'face 'elfeed-summary-unread-counts-face))))
	  ;; A little trick here: add a property field holding the feed ID; read back when jumping to that feed
	  (insert (propertize line 'feedid (plist-get item :feedid)))
	  (insert "\n")))
      (insert "End of summary.\n")
      (pop-to-buffer (current-buffer))
      (setf (point) (point-min)))))

(defun elfeed-summary-mode ()
  "Major mode for listing elfeed feeds and show some info about them.
\\{elfeed-search-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map elfeed-summary-mode-map)
  (setq major-mode 'elfeed-summary-mode
        mode-name "elfeed-summary"
        header-line-format (format elfeed-summary-line-format "Date" " Title" " Unread" "Total")
        truncate-lines t
        buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode)
  (elfeed-summary-update))
