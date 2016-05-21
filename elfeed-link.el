;;; elfeed-link.el --- Links to elfeed search and entry buffers, plus capturing for Org mode -*- lexical-binding: t; -*-
;;
;; This is free and unencumbered software released into the public domain.
;;
;;; Code:

(require 'org)

(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-show)
(require 'elfeed-search)

;;;###autoload
(defun elfeed-link-store ()
  "Store an Org link to the current elfeed search or entry buffer.

If point is on an http-style url in an entry buffer then that url
becomes the link for `org-store-link', otherwise it is an
'elfeed:...' url which is handled by `elfeed'.

Special properties stored about entries which can be used by
`org-capture' templates:

%:url                     The http url of the entry
%:enclosure-url           The http url of the first enclosure, if present
%:feed-url                The http url to the html list of entries
%:feed-title              The title of the feed
%:feed-author             The author of the feed
%:date-timestamp          The date the entry was published as an active Org date-time
%:date-timestamp-inactive The date the entry was published as an inactive Org date-time
%:tags                    The tags applied to the entry in Org 'tag1:tag2' format
"
  (require 'org)
  (cond ((eq major-mode 'elfeed-search-mode)
         (org-store-link-props
          :type "elfeed"
          :link (format "elfeed:%s" elfeed-search-filter)
          :description elfeed-search-filter))
        ((eq major-mode 'elfeed-show-mode)
         (let* ((entry elfeed-show-entry)
                (id (elfeed-entry-id entry))
                (link (format "elfeed:%s#%s" (car id) (cdr id)))
                (thing-url (get-text-property (point) 'shr-url))
                (thing-type (and thing-url
                                 (url-type (url-generic-parse-url thing-url)))))
           (if thing-type
               (org-store-link-props
                :type thing-type
                :link thing-url)
             (org-store-link-props
              :type "elfeed"
              :link link))
           (org-add-link-props
            :feed-url (elfeed-feed-url (elfeed-entry-feed entry))
            :feed-title (elfeed-feed-title (elfeed-entry-feed entry))
            :feed-author (elfeed-feed-author (elfeed-entry-feed entry))
            :url (elfeed-entry-link entry)
            :description (elfeed-entry-title entry)
            :date-timestamp (format-time-string
                             (org-time-stamp-format t)
                             (seconds-to-time (elfeed-entry-date entry)))
            :date-timestamp-inactive (format-time-string
                                      (org-time-stamp-format t t)
                                      (seconds-to-time (elfeed-entry-date entry)))
            :tags (mapconcat (lambda (s)
                               (let ((tag (symbol-name s)))
                                 (substring-no-properties tag 0 (length tag))))
                             (elfeed-entry-tags entry) ":")
            :enclosure-url (caar (elfeed-entry-enclosures entry))))
         t)))

(defun elfeed-link-filter-or-id (filter-or-id)
  "Parse the two kinds of links: search filter and entry id.
Entry IDs consist of the feed url and the entry id separated by
`#'. Everything else is a search filter."
  (if (string-match "\\(\\(?:file\\|https?\\)://[^#]+\\)#\\(.+\\)" filter-or-id)
      (cons (match-string 1 filter-or-id)
            (match-string 2 filter-or-id))
    filter-or-id))

;;;###autoload
(defun elfeed-link-open (path)
  "Jump to an elfeed entry or search."
  (let ((filter-or-id (elfeed-link-filter-or-id path)))
    (if (consp filter-or-id)
        (elfeed-show-entry (elfeed-db-get-entry filter-or-id))
      (switch-to-buffer (elfeed-search-buffer))
      (unless (eq major-mode 'elfeed-search-mode)
        (elfeed-search-mode))
      (elfeed-search-set-filter filter-or-id))))

;;;###autoload
(defun elfeed-link-export (path desc format)
  "Create the HTML export version of an ELFEED link specified by
PATH or DESC. Links for other export formats are handled in the
default Org way."
  (let* ((filter-or-id (elfeed-link-filter-or-id path))
         (entry (when (consp filter-or-id)
                  (elfeed-db-get-entry filter-or-id)))
         (url (when entry
                (or (elfeed-entry-link entry)
                    (elfeed-feed-url (elfeed-entry-feed entry))))))
    (if (eq format 'html)
        (if entry
            (format "<a href=\"%s\" class=\"elfeed-entry\">%s</a>"
                    (org-html-encode-plain-text url)
                    (org-html-encode-plain-text desc))
          (format "<i class=\"elfeed-search-filter\">%s</i>"
                  (org-html-encode-plain-text desc)))
      desc)))

;;;###autoload
(with-eval-after-load 'org
  (org-add-link-type "elfeed" #'elfeed-link-open #'elfeed-link-export)
  (add-hook 'org-store-link-functions #'elfeed-link-store))

(provide 'elfeed-link)

;;; elfeed-link.el ends here
