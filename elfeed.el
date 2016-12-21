;;; elfeed.el --- an Emacs Atom/RSS feed reader -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/elfeed

;;; Commentary:

;; Elfeed is a web feed client for Emacs, inspired by notmuch. See
;; the README for full documentation.

;;; History:

;; Version 2.0.1: features and fixes
;;   * Added `elfeed-curl-extra-arguments' customization
;;   * Use `x-get-selection' instead of `x-get-selection-value'
;;   * More flexible date handling (including Atom 0.3 support)
;;   * Various elfeed-web fixes
;; Version 2.0.0: new cURL fetching backend
;;   * Elfeed now uses cURL when available (`elfeed-use-curl')
;;   * Windows OS now supported when using cURL
;;   * Conditional GET (ETag, If-Modified-Since) when using cURL
;;   * Support for xml:base in Atom feeds
;;   * New options: `elfeed-set-max-connections', `elfeed-set-timeout'
;;   * New feed metadata: :canonical-url, :etag, :last-modified
;;   * New variable: `elfeed-log-level'
;;   * New database export option: `elfeed-csv-export'
;;   * Additional validation for `elfeed-feeds'
;; Version 1.4.1: features and fixes
;;   * Major bug fix: disable local variables when loading the index
;;   * New command `elfeed-show-play-enclosure' (requires emms)
;;   * Yank now works on regions in the search buffer
;;   * Feed structs now have author field filled out
;;   * New command `elfeed-search-set-feed-title'
;;   * New command `elfeed-search-set-entry-title'
;;   * Smarter handling of invalid timestamps
;;   * Following links in show mode (`elfeed-show-visit') takes a prefix arg
;; Version 1.4.0: features and fixes
;;   * New header built on Emacs' built-in buffer headers
;;   * New hook: `elfeed-new-entry-parse-hook'
;;   * Emacs' bookmark support (`bookmark-set', `bookmark-jump')
;;   * Emacs' desktop support (save/restore windows)
;;   * Custom faces in search listing via `elfeed-search-face-alist'
;;   * Dedicated log buffer, *elfeed-log*
;;   * Scoped updates with prefix argument to `elfeed-search-fetch'
;;   * Various bug fixes
;;   * Fixes to feed Unicode decoding
;; Version 1.3.0: features and fixes
;;   * `elfeed-search-face-alist' for custom entry faces
;;   * `display-local-help' (C-h .) support in search
;;   * Fixes to #n count filter
;; Version 1.2.0: features and fixes
;;   * Switched to url-queue (see `url-queue-timeout')
;;   * New #n filter for limiting results to first n entries
;;   * Faster live filtering
;;   * `elfeed-version'
;;   * Enclosure downloading
;;   * Database size optimizations
;;   * Search listing is more responsive to updates
;;   * `elfeed-http-error-hooks', `elfeed-parse-error-hooks'
;;   * Various bug fixes
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
(require 'url-queue)
(require 'elfeed-log)
(require 'elfeed-curl)

(defgroup elfeed nil
  "An Emacs web feed reader."
  :group 'comm)

(defconst elfeed-version "2.0.1")

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

(defcustom elfeed-feed-functions
  '(elfeed-get-link-at-point
    elfeed-get-url-at-point
    elfeed-clipboard-get)
  "List of functions to use to get possible feeds for `elfeed-add-feed'.
Each function should accept no arguments, and return a string or nil."
  :group 'elfeed
  :type 'hook
  :options '(elfeed-get-link-at-point
             elfeed-get-url-at-point
             elfeed-clipboard-get))

(defcustom elfeed-use-curl
  (not (null (executable-find elfeed-curl-program-name)))
  "If non-nil, fetch feeds using curl instead of `url-retrieve'."
  :group 'elfeed
  :type 'bool)

(defcustom elfeed-user-agent (format "Emacs Elfeed %s" elfeed-version)
  "User agent string to use for Elfeed (requires `elfeed-use-curl')."
  :group 'elfeed
  :type 'string)

(provide 'elfeed)

(require 'elfeed-search)
(require 'elfeed-lib)
(require 'elfeed-db)
(require 'elfeed-csv)

(defcustom elfeed-initial-tags '(unread)
  "Initial tags for new entries."
  :group 'elfeed
  :type '(repeat symbol))

;; Fetching:

(defvar elfeed-http-error-hooks ()
  "Hooks to run when an http connection error occurs.
It is called with 2 arguments. The first argument is the url of
the failing feed. The second argument is the http status code.")

(defvar elfeed-parse-error-hooks ()
  "Hooks to run when an error occurs during the parsing of a feed.
It is called with 2 arguments. The first argument is the url of
the failing feed. The second argument is the error message .")

(defvar elfeed-update-hooks ()
  "Hooks to run any time a feed update has completed a request.
It is called with 1 argument: the URL of the feed that was just
updated. The hook is called even when no new entries were
found.")

(defun elfeed-queue-count-active ()
  "Return the number of items in process."
  (if elfeed-use-curl
      elfeed-curl-queue-active
    (cl-count-if #'url-queue-buffer url-queue)))

(defun elfeed-queue-count-total ()
  "Return the number of items in process."
  (if elfeed-use-curl
      (+ (length elfeed-curl-queue) elfeed-curl-queue-active)
    (length url-queue)))

(defun elfeed-set-max-connections (n)
  "Limit the maximum number of concurrent connections to N."
  (if elfeed-use-curl
      (setf elfeed-curl-max-connections n)
    (setf url-queue-parallel-processes n)))

(defun elfeed-get-max-connections ()
  "Get the maximum number of concurrent connections."
  (if elfeed-use-curl
      elfeed-curl-max-connections
    url-queue-parallel-processes))

(defun elfeed-set-timeout (seconds)
  "Limit the time for fetching a feed to SECONDS."
  (if elfeed-use-curl
      (setf elfeed-curl-timeout seconds)
    (setf url-queue-timeout seconds)))

(defun elfeed-get-timeout ()
  "Get the time limit for fetching feeds in SECONDS."
  (if elfeed-use-curl
      elfeed-curl-timeout
    url-queue-timeout))

(defmacro elfeed-with-fetch (url &rest body)
  "Asynchronously run BODY in a buffer with the contents from
URL. This macro is anaphoric, with STATUS referring to the status
from `url-retrieve'."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl) ; capture current value in closure
          (cb (lambda (status) ,@body)))
     (if elfeed-use-curl
         (let* ((feed (elfeed-db-get-feed url))
                (last-modified (elfeed-meta feed :last-modified))
                (etag (elfeed-meta feed :etag))
                (headers `(("User-Agent" . ,elfeed-user-agent))))
           (when etag
             (push `("If-None-Match" . ,etag) headers))
           (when last-modified
             (push `("If-Modified-Since" . ,last-modified) headers))
           (elfeed-curl-enqueue ,url cb headers))
       (url-queue-retrieve ,url cb () t t))))

(defun elfeed-unjam ()
  "Manually clear the connection pool when connections fail to timeout.
This is a workaround for issues in `url-queue-retrieve'."
  (interactive)
  (if elfeed-use-curl
      (setf elfeed-curl-queue nil
            elfeed-curl-queue-active 0)
    (let ((fails (mapcar #'url-queue-url url-queue)))
      (when fails
        (elfeed-log 'warn "Elfeed aborted feeds: %s"
                    (mapconcat #'identity fails " ")))
      (setf url-queue nil)))
  (elfeed-search-update :force))

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

(defun elfeed--atom-content (entry)
  "Get content string from ENTRY."
  (let ((content-type (xml-query '(content :type) entry)))
    (if (equal content-type "xhtml")
        (with-temp-buffer
          (let ((xhtml (cddr (xml-query '(content) entry))))
            (dolist (element xhtml)
              (if (stringp element)
                  (insert element)
                (elfeed-xml-unparse element))))
          (buffer-string))
      (let ((all-content
             (or (xml-query-all '(content *) entry)
                 (xml-query-all '(summary *) entry))))
        (when all-content
          (apply #'concat all-content))))))

(defvar elfeed-new-entry-parse-hook '()
  "Hook to be called after parsing a new entry.

Take three arguments: the feed TYPE, the XML structure for the
entry, and the Elfeed ENTRY object. Return value is ignored, and
is called for side-effects on the ENTRY object.")

(defun elfeed-entries-from-atom (url xml)
  "Turn parsed Atom content into a list of elfeed-entry structs."
  (let* ((feed-id url)
         (namespace (elfeed-url-to-namespace url))
         (feed (elfeed-db-get-feed feed-id))
         (title (elfeed-cleanup (xml-query '(feed title *) xml)))
         (author (elfeed-cleanup (xml-query '(feed author name *) xml)))
         (xml-base (or (xml-query '(feed :xml:base) xml) url))
         (autotags (elfeed-feed-autotags url)))
    (setf (elfeed-feed-url feed) url
          (elfeed-feed-title feed) title
          (elfeed-feed-author feed) author)
    (cl-loop for entry in (xml-query-all '(feed entry) xml) collect
             (let* ((title (or (xml-query '(title *) entry) ""))
                    (xml-base (elfeed-update-location
                               xml-base (xml-query '(:xml:base) (list entry))))
                    (anylink (xml-query '(link :href) entry))
                    (altlink (xml-query '(link [rel "alternate"] :href) entry))
                    (link (elfeed-update-location
                           xml-base (or altlink anylink)))
                    (date (or (xml-query '(published *) entry)
                              (xml-query '(updated *) entry)
                              (xml-query '(date *) entry)
                              (xml-query '(modified *) entry) ; Atom 0.3
                              (xml-query '(issued *) entry))) ; Atom 0.3
                    (author-name (or (xml-query '(author name *) entry)
                                     ;; Dublin Core
                                     (xml-query '(creator *) entry)))
                    (author-email (xml-query '(author email *) entry))
                    (author (cond ((and author-name author-email)
                                   (format "%s <%s>" author-name author-email))
                                  (author-name)))
                    (categories (mapcar #'cl-cdaadr
                                        (xml-query-all '(category) entry)))
                    (content (elfeed--atom-content entry))
                    (id (or (xml-query '(id *) entry) link
                            (elfeed-generate-id content)))
                    (type (or (xml-query '(content :type) entry)
                              (xml-query '(summary :type) entry)
                              ""))
                    (tags (elfeed-normalize-tags autotags elfeed-initial-tags))
                    (content-type (if (string-match-p "html" type) 'html nil))
                    (etags (xml-query-all '(link [rel "enclosure"]) entry))
                    (enclosures
                     (cl-loop for enclosure in etags
                              for wrap = (list enclosure)
                              for href = (xml-query '(:href) wrap)
                              for type = (xml-query '(:type) wrap)
                              for length = (xml-query '(:length) wrap)
                              collect (list href type length)))
                    (db-entry (elfeed-entry--create
                               :title (elfeed-cleanup title)
                               :feed-id feed-id
                               :id (cons namespace (elfeed-cleanup id))
                               :link (elfeed-cleanup link)
                               :tags tags
                               :date (or (elfeed-float-time date) (float-time))
                               :content content
                               :enclosures enclosures
                               :content-type content-type
                               :meta `(,@(when author
                                           (list :author author))
                                       ,@(when categories
                                           (list :categories categories))))))
               (dolist (hook elfeed-new-entry-parse-hook)
                 (funcall hook :atom entry db-entry))
               db-entry))))

(defun elfeed-entries-from-rss (url xml)
  "Turn parsed RSS content into a list of elfeed-entry structs."
  (let* ((feed-id url)
         (namespace (elfeed-url-to-namespace url))
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
                    (author (or (xml-query '(author *) item)
                                ;; Dublin Core
                                (xml-query '(creator *) item)))
                    (categories (xml-query-all '(category *) item))
                    (content (or (xml-query-all '(encoded *) item)
                                 (xml-query-all '(description *) item)))
                    (description (apply #'concat content))
                    (id (or guid link (elfeed-generate-id description)))
                    (full-id (cons namespace (elfeed-cleanup id)))
                    (original (elfeed-db-get-entry full-id))
                    (original-date (and original (elfeed-entry-date original)))
                    (tags (elfeed-normalize-tags autotags elfeed-initial-tags))
                    (etags (xml-query-all '(enclosure) item))
                    (enclosures
                     (cl-loop for enclosure in etags
                              for wrap = (list enclosure)
                              for url = (xml-query '(:url) wrap)
                              for type = (xml-query '(:type) wrap)
                              for length = (xml-query '(:length) wrap)
                              collect (list url type length)))
                    (db-entry (elfeed-entry--create
                               :title (elfeed-cleanup title)
                               :id full-id
                               :feed-id feed-id
                               :link (elfeed-cleanup link)
                               :tags tags
                               :date (elfeed-new-date-for-entry
                                      original-date date)
                               :enclosures enclosures
                               :content description
                               :content-type 'html
                               :meta `(,@(when author
                                           (list :author author))
                                       ,@(when categories
                                           (list :categories categories))))))
               (dolist (hook elfeed-new-entry-parse-hook)
                 (funcall hook :rss item db-entry))
               db-entry))))

(defun elfeed-entries-from-rss1.0 (url xml)
  "Turn parsed RSS 1.0 content into a list of elfeed-entry structs."
  (let* ((feed-id url)
         (namespace (elfeed-url-to-namespace url))
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
                    (full-id (cons namespace (elfeed-cleanup id)))
                    (original (elfeed-db-get-entry full-id))
                    (original-date (and original (elfeed-entry-date original)))
                    (tags (elfeed-normalize-tags autotags elfeed-initial-tags))
                    (db-entry (elfeed-entry--create
                               :title (elfeed-cleanup title)
                               :id full-id
                               :feed-id feed-id
                               :link (elfeed-cleanup link)
                               :tags tags
                               :date (elfeed-new-date-for-entry
                                      original-date date)
                               :content description
                               :content-type 'html)))
               (dolist (hook elfeed-new-entry-parse-hook)
                 (funcall hook :rss1.0 item db-entry))
               db-entry))))

(defun elfeed-feed-list ()
  "Return a flat list version of `elfeed-feeds'.
Only a list of strings will be returned."
  ;; Validate elfeed-feeds and fail early rather than asynchronously later.
  (dolist (feed elfeed-feeds)
    (unless (cl-typecase feed
              (list (and (stringp (car feed))
                         (cl-every #'symbolp (cdr feed))))
              (string t))
      (error "elfeed-feeds malformed, bad entry: %S" feed)))
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
  (elfeed-log 'error "%s: %S" url status))

(defun elfeed-handle-parse-error (url error)
  "Handle parse error during parsing of URL with ERROR message."
  (cl-incf (elfeed-meta (elfeed-db-get-feed url) :failures 0))
  (run-hook-with-args 'elfeed-parse-error-hooks url error)
  (elfeed-log 'error "%s: %s" url error))

(defun elfeed-update-feed (url)
  "Update a specific feed."
  (interactive (list (completing-read "Feed: " (elfeed-feed-list))))
  (elfeed-with-fetch url
    (if (or (and use-curl (null status)) ; nil = error
            (and (not use-curl) (eq (car status) :error)))
        (let ((print-escape-newlines t))
          (elfeed-handle-http-error
           url (if use-curl elfeed-curl-error-message status)))
      (condition-case error
          (let ((feed (elfeed-db-get-feed url)))
            (unless use-curl
              (elfeed-move-to-first-empty-line)
              (set-buffer-multibyte t))
            (unless (eql elfeed-curl-status-code 304)
              ;; Update Last-Modified and Etag
              (setf (elfeed-meta feed :last-modified)
                    (cdr (assoc "last-modified" elfeed-curl-headers))
                    (elfeed-meta feed :etag)
                    (cdr (assoc "etag" elfeed-curl-headers)))
              (if (equal url elfeed-curl-location)
                  (setf (elfeed-meta feed :canonical-url) nil)
                (setf (elfeed-meta feed :canonical-url) elfeed-curl-location))
              (let* ((xml (elfeed-xml-parse-region (point) (point-max)))
                     (entries (cl-case (elfeed-feed-type xml)
                                (:atom (elfeed-entries-from-atom url xml))
                                (:rss (elfeed-entries-from-rss url xml))
                                (:rss1.0 (elfeed-entries-from-rss1.0 url xml))
                                (otherwise
                                 (error (elfeed-handle-parse-error
                                         url "Unknown feed type."))))))
                (elfeed-db-add entries))))
        (error (elfeed-handle-parse-error url error))))
    (unless use-curl
      (kill-buffer))
    (run-hook-with-args 'elfeed-update-hooks url)))

(defun elfeed-candidate-feeds ()
  "Return a list of possible feeds from `elfeed-feed-functions'."
  (let (res)
    (run-hook-wrapped
     'elfeed-feed-functions
     (lambda (fun)
       (let* ((val (elfeed-cleanup (funcall fun))))
         (when (and (not (zerop (length val)))
                    (elfeed-looks-like-url-p val))
           (cl-pushnew val res :test #'equal)))
       nil))
    (nreverse res)))

(defun elfeed-add-feed (url)
  "Manually add a feed to the database."
  (interactive
   (list
    (let* ((feeds (elfeed-candidate-feeds))
           (prompt (if feeds (concat "URL (default " (car feeds)  "): ")
                     "URL: "))
           (res (read-from-minibuffer prompt nil nil nil nil feeds)))
      (if (not (zerop (length (elfeed-cleanup res)))) res
        (if feeds (car feeds)
          (user-error "No feed to add"))))))
  (cl-pushnew url elfeed-feeds)
  (when (called-interactively-p 'any)
    (customize-save-variable 'elfeed-feeds elfeed-feeds))
  (elfeed-update-feed url)
  (elfeed-search-update :force))

;;;###autoload
(defun elfeed-update ()
  "Update all the feeds in `elfeed-feeds'."
  (interactive)
  (elfeed-log 'info "Elfeed update: %s"
              (format-time-string "%B %e %Y %H:%M:%S %Z"))
  (mapc #'elfeed-update-feed (elfeed--shuffle (elfeed-feed-list)))
  (elfeed-search-update :force)
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
        (elfeed-log 'notice "%d feeds loaded from %s" (length feeds) file)))))

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
