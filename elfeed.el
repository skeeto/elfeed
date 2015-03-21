;;; elfeed.el --- an Emacs Atom/RSS feed reader -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/elfeed

;;; Commentary:

;; Elfreed is a web feed client for Emacs, inspired by notmuch. See
;; the README for full documentation.

;; Elfreed requires Emacs 24 (lexical closures).

;;; History:

;; Version 1.1.2: fixes
;;   * Fixed support for non-HTTP protocols
;;   * Add ! search syntax
;;   * Add elfeed-unjam
;;   * Combine regexp search terms by AND instead of OR
;;   * Link navigation keybindings (tab)
;;   * Add elfeed-show-truncate-long-urls
;;   * Add elfeed-search-filter customization
;;   * Various bug fixes
;; Version 1.1.1: fixes
;;   * Fix database corruption issue
;;   * Properly handle URLs from XML
;;   * Slightly better RSS date guessing
;;   * User interface tweaks
;;   * Add `elfeed-sort-order'
;;   * Use tab and backtab to move between links
;; Version 1.1.0: features and fixes
;;   * Autotagging support
;;   * Better database performance
;;   * Database packing
;;   * Arbitrary struct metadata
;;   * Added `elfeed-search-clipboard-type'
;;   * Update to cl-lib from cl
;;   * Lots of bug fixes
;; Version 1.0.1: features and fixes
;;   * Live filter editing
;;   * Support for RSS 1.0
;;   * OPML import/export
;;   * Fix multibyte support (thanks cellscape)
;;   * Fix date-change database corruption
;;   * Add n and p bindings to elfeed-search, like notmuch
;;   * Friendlier intro header
;;   * Automated builds
;;   * Lots of small bug fixes
;; Version 1.0.0: initial public release

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'xml-query)
(require 'url-parse)

(defgroup elfeed nil
  "An Emacs web feed reader."
  :group 'comm)

(defcustom elfeed-feeds ()
  "List of all feeds that Elfeed should follow. You must add your
feeds to this list.

In its simplest form this will be a list of strings of feed URLs.
Items in this list can also be list whose car is the feed URL
and cdr is a list of symbols to be applied to all discovered
entries as tags (\"autotags\"). For example,

  (setq elfeed-feeds '(\"http://foo/\"
                       \"http://bar/\"
                       (\"http://baz/\" comic)))

All entries from the \"baz\" feed will be tagged as \"comic\"
when they are first discovered."
  :group 'elfeed
  :type '(repeat (choice string
                         (cons string (repeat symbol)))))

(provide 'elfeed)

(require 'elfeed-search)
(require 'elfeed-lib)
(require 'elfeed-db)

(defcustom elfeed-initial-tags '(unread)
  "Initial tags for new entries."
  :group 'elfeed
  :type '(repeat symbol))

;; Fetching:

(defcustom elfeed-max-connections
  ;; Windows Emacs cannot open many sockets at once.
  (if (or (not (and (fboundp 'gnutls-available-p) (gnutls-available-p)))
          (eq system-type 'windows-nt))
      1
    4)
  "The maximum number of feeds to be fetched in parallel."
  :group 'elfeed
  :type 'integer)

(defvar elfeed-http-error-hooks nil
  "Hooks to run when an http connection error occurs.
It is called with 2 arguments. The first argument is the url of
the failing feed. The second argument is the http status code.")

(defvar elfeed-parse-error-hooks nil
  "Hooks to run when an error occurs during the parsing of a feed.
It is called with 2 arguments. The first argument is the url of
the failing feed. The second argument is the error message .")

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
      (cl-destructuring-bind (_ url cb) request
        (push request elfeed-connections)
        (condition-case error
            (url-retrieve url cb nil :silent :no-cookies)
          (error (with-temp-buffer (funcall cb (list :error error)))))))))

(defun elfeed--wrap-callback (id cb)
  "Return a function that manages the elfeed queue."
  (let ((once nil))
    (lambda (status)
      (unless once
        (setf once t) ;; url-retrieve bug#20159 workaround
        (unwind-protect
            (funcall cb status)
          (setf elfeed-connections
                (cl-delete id elfeed-connections :key #'car))
          (elfeed--check-queue))))))

(defun elfeed-fetch (url callback)
  "Basically wraps `url-retrieve' but uses the connection limiter."
  (let* ((id (cl-incf elfeed--connection-counter))
         (cb (elfeed--wrap-callback id callback)))
    (push (list id url cb) elfeed-waiting)
    (elfeed--check-queue)))

(defmacro with-elfeed-fetch (url &rest body)
  "Asynchronously run BODY in a buffer with the contents from
URL. This macro is anaphoric, with STATUS referring to the status
from `url-retrieve'."
  (declare (indent defun))
  `(elfeed-fetch ,url (lambda (status) ,@body (kill-buffer))))

(defun elfeed-unjam ()
  "Manually clear the connection pool when connections fail to timeout.
This is a short-term workaround for connection handling issues."
  (interactive)
  (let ((fails (mapcar #'cl-second elfeed-connections)))
    (when fails
      (message "Elfeed aborted feeds: %s" (mapconcat #'identity fails " ")))
    (setf elfeed-connections nil)
    (elfeed--check-queue)))

;; Parsing:

(defun elfeed-feed-type (content)
  "Return the feed type given the parsed content (:atom, :rss) or
NIL for unknown."
  (let ((top (xml-query-strip-ns (caar content))))
    (cadr (assoc top '((feed :atom)
                       (rss :rss)
                       (RDF :rss1.0))))))

(defun elfeed-generate-id (&optional content)
  "Generate an ID based on CONTENT or from the current time."
  (concat "urn:sha1:" (sha1 (format "%s" (or content (float-time))))))

(defun elfeed-entries-from-atom (url xml)
  "Turn parsed Atom content into a list of elfeed-entry structs."
  (let* ((feed-id url)
         (feed (elfeed-db-get-feed feed-id))
         (title (elfeed-cleanup (xml-query '(feed title *) xml)))
         (autotags (elfeed-feed-autotags url)))
    (setf (elfeed-feed-url feed) url
          (elfeed-feed-title feed) title)
    (cl-loop for entry in (xml-query-all '(feed entry) xml) collect
             (let* ((title (or (xml-query '(title *) entry) ""))
                    (anylink (xml-query '(link :href) entry))
                    (altlink (xml-query '(link [rel "alternate"] :href) entry))
                    (link (or altlink anylink))
                    (date (or (xml-query '(published *) entry)
                              (xml-query '(updated *) entry)
                              (xml-query '(date *) entry)))
                    (content
                     (let ((all-content
                            (or (xml-query-all '(content *) entry)
                                (xml-query-all '(summary *) entry))))
                       (when all-content
                         (apply #'concat all-content))))
                    (id (or (xml-query '(id *) entry) link
                            (elfeed-generate-id content)))
                    (type (or (xml-query '(content :type) entry)
                              (xml-query '(summary :type) entry)
                              ""))
                    (etags (xml-query-all '(link [rel "enclosure"]) entry))
                    (enclosures
                     (cl-loop for enclosure in etags
                              for wrap = (list enclosure)
                              for href = (xml-query '(:href) wrap)
                              for type = (xml-query '(:type) wrap)
                              for length = (xml-query '(:length) wrap)
                              collect (list href type length))))
               (elfeed-entry--create
                :title (elfeed-cleanup title)
                :feed-id feed-id
                :id (cons feed-id (elfeed-cleanup id))
                :link (elfeed-cleanup link)
                :tags (elfeed-normalize-tags autotags elfeed-initial-tags)
                :date (elfeed-float-time date)
                :content content
                :enclosures enclosures
                :content-type (if (string-match-p "html" type) 'html nil))))))

(defun elfeed-entries-from-rss (url xml)
  "Turn parsed RSS content into a list of elfeed-entry structs."
  (let* ((feed-id url)
         (feed (elfeed-db-get-feed feed-id))
         (title (elfeed-cleanup (xml-query '(rss channel title *) xml)))
         (autotags (elfeed-feed-autotags url)))
    (setf (elfeed-feed-url feed) url
          (elfeed-feed-title feed) title)
    (cl-loop for item in (xml-query-all '(rss channel item) xml) collect
             (let* ((title (or (xml-query '(title *) item) ""))
                    (guid (xml-query '(guid *) item))
                    (link (or (xml-query '(link *) item) guid))
                    (date (or (xml-query '(pubDate *) item)
                              (xml-query '(date *) item)))
                    (description
                     (apply #'concat (xml-query-all '(description *) item)))
                    (id (or guid link (elfeed-generate-id description)))
                    (full-id (cons feed-id (elfeed-cleanup id)))
                    (original (elfeed-db-get-entry full-id))
                    (etags (xml-query-all '(enclosure) item))
                    (enclosures
                     (cl-loop for enclosure in etags
                              for wrap = (list enclosure)
                              for url = (xml-query '(:url) wrap)
                              for type = (xml-query '(:type) wrap)
                              for length = (xml-query '(:length) wrap)
                              collect (list url type length))))
               (elfeed-entry--create
                :title (elfeed-cleanup title)
                :id full-id
                :feed-id feed-id
                :link (elfeed-cleanup link)
                :tags (elfeed-normalize-tags autotags elfeed-initial-tags)
                :date (if (and original (null date))
                          (elfeed-entry-date original)
                        (elfeed-float-time date))
                :enclosures enclosures
                :content description
                :content-type 'html)))))

(defun elfeed-entries-from-rss1.0 (url xml)
  "Turn parsed RSS 1.0 content into a list of elfeed-entry structs."
  (let* ((feed-id url)
         (feed (elfeed-db-get-feed feed-id))
         (title (elfeed-cleanup (xml-query '(RDF channel title *) xml)))
         (autotags (elfeed-feed-autotags url)))
    (setf (elfeed-feed-url feed) url
          (elfeed-feed-title feed) title)
    (cl-loop for item in (xml-query-all '(RDF item) xml) collect
             (let* ((title (or (xml-query '(title *) item) ""))
                    (link (xml-query '(link *) item))
                    (date (or (xml-query '(pubDate *) item)
                              (xml-query '(date *) item)))
                    (description
                     (apply #'concat (xml-query-all '(description *) item)))
                    (id (or link (elfeed-generate-id description)))
                    (full-id (cons feed-id (elfeed-cleanup id)))
                    (original (elfeed-db-get-entry full-id)))
               (elfeed-entry--create
                :title (elfeed-cleanup title)
                :id full-id
                :feed-id feed-id
                :link (elfeed-cleanup link)
                :tags (elfeed-normalize-tags autotags elfeed-initial-tags)
                :date (if (and original (null date))
                          (elfeed-entry-date original)
                        (elfeed-float-time date))
                :content description
                :content-type 'html)))))

(defun elfeed-feed-list ()
  "Return a flat list version of `elfeed-feeds'.
Only a list of strings will be returned."
  (cl-loop for feed in elfeed-feeds
           when (listp feed) collect (car feed)
           else collect feed))

(defun elfeed-feed-autotags (url-or-feed)
  "Return tags to automatically apply to all entries from URL-OR-FEED."
  (let ((url (if (elfeed-feed-p url-or-feed)
                 (or (elfeed-feed-url url-or-feed)
                     (elfeed-feed-id url-or-feed))
               url-or-feed)))
    (mapcar #'elfeed-keyword->symbol (cdr (assoc url elfeed-feeds)))))

(defun elfeed-handle-http-error (url status)
  "Handle an http error during retrieval of URL with STATUS code."
  (cl-incf (elfeed-meta (elfeed-db-get-feed url) :failures 0))
  (run-hook-with-args 'elfeed-http-error-hooks url status)
  (message "Elfeed fetch failed for %s: %S" url status))

(defun elfeed-handle-parse-error (url error)
  "Handle parse error during parsing of URL with ERROR message."
  (cl-incf (elfeed-meta (elfeed-db-get-feed url) :failures 0))
  (run-hook-with-args 'elfeed-parse-error-hooks url error)
  (message "Elfeed parse failed for %s: %s" url error))

(defun elfeed-update-feed (url)
  "Update a specific feed."
  (interactive (list (completing-read "Feed: " (elfeed-feed-list))))
  (with-elfeed-fetch url
    (if (and status (eq (car status) :error))
        (let ((print-escape-newlines t))
          (elfeed-handle-http-error url status))
      (condition-case error
          (progn
            (elfeed-move-to-first-empty-line)
            (set-buffer-multibyte t)
            (let* ((xml (elfeed-xml-parse-region (point) (point-max)))
                   (entries (cl-case (elfeed-feed-type xml)
                              (:atom (elfeed-entries-from-atom url xml))
                              (:rss (elfeed-entries-from-rss url xml))
                              (:rss1.0 (elfeed-entries-from-rss1.0 url xml))
                              (otherwise
                               (error (elfeed-handle-parse-error
                                       url "Unknown feed type."))))))
              (elfeed-db-add entries)))
        (error (elfeed-handle-parse-error url error))))))

(defun elfeed-add-feed (url)
  "Manually add a feed to the database."
  (interactive (list
                (let ((clipboard (elfeed-cleanup (elfeed-clipboard-get))))
                  (read-from-minibuffer
                   "URL: " (when (elfeed-looks-like-url-p clipboard)
                             clipboard)))))
  (cl-pushnew url elfeed-feeds)
  (when (called-interactively-p 'any)
    (customize-save-variable 'elfeed-feeds elfeed-feeds))
  (elfeed-update-feed url))

;;;###autoload
(defun elfeed-update ()
  "Update all the feeds in `elfeed-feeds'."
  (interactive)
  (message "Elfeed update: %s" (format-time-string "%B %e %Y %H:%M:%S %Z"))
  (mapc #'elfeed-update-feed (elfeed-feed-list))
  (elfeed-db-save))

;;;###autoload
(defun elfeed ()
  "Enter elfeed."
  (interactive)
  (switch-to-buffer (elfeed-search-buffer))
  (unless (eq major-mode 'elfeed-search-mode)
    (elfeed-search-mode))
  (elfeed-search-update))

;; New entry filtering

(cl-defun elfeed-make-tagger
    (&key feed-title feed-url entry-title entry-link after before
          add remove callback)
  "Create a function that adds or removes tags on matching entries.

FEED-TITLE, FEED-URL, ENTRY-TITLE, and ENTRY-LINK are regular
expressions or a list (not <regex>), which indicates a negative
match. AFTER and BEFORE are relative times (see
`elfeed-time-duration'). Entries must match all provided
expressions. If an entry matches, add tags ADD and remove tags
REMOVE.

Examples,

  (elfeed-make-tagger :feed-url \"youtube\\\\.com\"
                      :add '(video youtube))

  (elfeed-make-tagger :before \"1 week ago\"
                      :remove 'unread)

  (elfeed-make-tagger :feed-url \"example\\\\.com\"
                      :entry-title '(not \"something interesting\")
                      :add 'junk)

The returned function should be added to `elfeed-new-entry-hook'."
  (let ((after-time  (and after  (elfeed-time-duration after)))
        (before-time (and before (elfeed-time-duration before))))
    (when (and add (symbolp add)) (setf add (list add)))
    (when (and remove (symbolp remove)) (setf remove (list remove)))
    (lambda (entry)
      (let ((feed (elfeed-entry-feed entry))
            (date (elfeed-entry-date entry))
            (case-fold-search t))
        (cl-flet ((match (r s)
                         (or (null r)
                             (if (listp r)
                                 (not (string-match-p (cl-second r) s))
                               (string-match-p r s)))))
          (when (and
                 (match feed-title  (elfeed-feed-title  feed))
                 (match feed-url    (elfeed-feed-url    feed))
                 (match entry-title (elfeed-entry-title entry))
                 (match entry-link  (elfeed-entry-link  entry))
                 (or (not after-time)  (> date (- (float-time) after-time)))
                 (or (not before-time) (< date (- (float-time) before-time))))
            (when add
              (apply #'elfeed-tag entry add))
            (when remove
              (apply #'elfeed-untag entry remove))
            (when callback
              (funcall callback entry))
            entry))))))

;; OPML

(defun elfeed--parse-opml (xml)
  "Parse XML (from `xml-parse-region') into `elfeed-feeds' list."
  (cl-loop for (tag attr . content) in (cl-remove-if-not #'listp xml)
           count tag into work-around-bug  ; bug#15326
           when (assoc 'xmlUrl attr) collect (cdr it)
           else append (elfeed--parse-opml content)))

;;;###autoload
(defun elfeed-load-opml (file)
  "Load feeds from an OPML file into `elfeed-feeds'.
When called interactively, the changes to `elfeed-feeds' are
saved to your customization file."
  (interactive "fOPML file: ")
  (let* ((xml (xml-parse-file file))
         (feeds (elfeed--parse-opml xml))
         (full (append feeds elfeed-feeds)))
    (prog1 (setf elfeed-feeds (cl-delete-duplicates full :test #'string=))
      (when (called-interactively-p 'any)
        (customize-save-variable 'elfeed-feeds elfeed-feeds)
        (message "%d feeds loaded from %s" (length feeds) file)))))

;;;###autoload
(defun elfeed-export-opml (file)
  "Export the current feed listing to OPML-formatted FILE."
  (interactive "FOutput OPML file: ")
  (with-temp-file file
    (let ((standard-output (current-buffer)))
      (princ "<?xml version=\"1.0\"?>\n")
      (xml-print
       `((opml ((version . "1.0"))
               (head () (title () "Elfeed Export"))
               (body ()
                     ,@(cl-loop for url in (elfeed-feed-list)
                                for feed = (elfeed-db-get-feed url)
                                for title = (or (elfeed-feed-title feed) "")
                                collect `(outline ((xmlUrl . ,url)
                                                   (title . ,title)))))))))))

;;; elfeed.el ends here
