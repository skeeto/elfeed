;;; elfeed-link.el --- misc functions for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Code for integration with org-mode.

;; To use, add (require 'elfeed-link) somewhere in your configuration.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'elfeed-db)
(require 'elfeed-show)
(require 'elfeed-search)

;;;###autoload
(defun elfeed-link-store-link ()
  "Store a link to an elfeed search or entry buffer.

When storing a link to an entry, automatically extract all the
entry metadata. These can be used in the capture templates as
`%:keyword` expansion.

List of available keywords, when store from an Elfeed search:
- `type`        : Type of Org-mode link
- `link`        : Org-mode link to this search, also available
                  with %a, %A, %l and %L
- `description` : The search filter


List of available keywords, when store from an Elfeed entry:
- `type`                    : Type of Org-mode link
- `link`                    : Org-mode link to this entry, also available
                              with %a, %A, %l and %L
- `title`                   : Feed entry title
- `description`             : Feed entry description, same as title
- `external-link`           : Feed entry external link
- `date`                    : Date time of the feed entry publication, in
                              full ISO 8601 format
- `date-timestamp`          : Date time of the feed entry publication, in
                              Org-mode active timestamp format
- `date-inactive-timestamp` : Date time of the feed entry publication, in
                              Org-mode inactive timestamp format
- `authors`                 : List of feed entry authors names, joint by a
                              comma
- `tags`                    : List of feed entry tags, in Org-mode tags
                              format
- `content`                 : Content of the feed entry
- `feed-title`              : Title of the feed
- `feed-external-link`      : Feed external link
- `feed-authors`            : List of feed authors names, joint by a comma

If `content` type is HTML, it is automatically embedded into an
Org-mode HTML quote."
  (cond ((derived-mode-p 'elfeed-search-mode)
         (funcall (if (fboundp 'org-link-store-props)
                      #'org-link-store-props
                    (with-no-warnings #'org-store-link-props))
          :type "elfeed"
          :link (format "elfeed:%s" elfeed-search-filter)
          :description elfeed-search-filter))
        ((derived-mode-p 'elfeed-show-mode)
         (funcall (if (fboundp 'org-link-store-props)
                      #'org-link-store-props
                    (with-no-warnings #'org-store-link-props))
          :type "elfeed"
          :link (format "elfeed:%s#%s"
                        (car (elfeed-entry-id elfeed-show-entry))
                        (cdr (elfeed-entry-id elfeed-show-entry)))
          :description (elfeed-entry-title elfeed-show-entry)
          :title (elfeed-entry-title elfeed-show-entry)
          :external-link (elfeed-entry-link elfeed-show-entry)
          ;; Format date to full ISO 8601 format
          :date (format-time-string
                 "%FT%T"
                 (elfeed-entry-date elfeed-show-entry))
          ;; Concatenate authors names
          :authors (mapconcat #'identity
                              ;; Loop on each author and extract its name
                              ;; Authors list get from Elfeed entry's meta
                              (cl-loop for author
                                       in (plist-get (elfeed-entry-meta elfeed-show-entry) :authors)
                                       collect (plist-get author :name))
                              ", ") ;; Join names using a comma
          ;; Concatenate tags in Org-mode tags format
          :tags (format ":%s:"
                        (mapconcat #'symbol-name
                                   (elfeed-entry-tags elfeed-show-entry)
                                   ":"))
          ;; Prepare support of different content type, only HTML for now
          :content (pcase (elfeed-entry-content-type elfeed-show-entry)
                     (`html
                      ;; Embed the text into Org-mode HTML quote
                      (format
                       "#+BEGIN_EXPORT html\n%s\n#+END_EXPORT"
                       (elfeed-deref (elfeed-entry-content elfeed-show-entry)))))
          :feed-title (elfeed-feed-title (elfeed-entry-feed elfeed-show-entry))
          :feed-external-link (elfeed-feed-url (elfeed-entry-feed elfeed-show-entry))
          ;; Concatenate feed authors names
          :feed-authors (mapconcat #'identity
                                   ;; Loop on each feed author and extract its name
                                   ;; Authors list get from Elfeed feed
                                   (cl-loop for author
                                            in (elfeed-feed-author(elfeed-entry-feed elfeed-show-entry))
                                            collect (plist-get author :name))
                                   ", ") ;; Join names using a comma
          ))))

;;;###autoload
(defun elfeed-link-open (filter-or-id)
  "Jump to an elfeed entry or search.

Depending on what FILTER-OR-ID looks like, we jump to either
search buffer or show a concrete entry."
  (if (string-match "\\([^#]+\\)#\\(.+\\)" filter-or-id)
      (elfeed-show-entry (elfeed-db-get-entry
                          (cons (match-string 1 filter-or-id)
                                (match-string 2 filter-or-id))))
    (elfeed)
    (elfeed-search-set-filter filter-or-id)))

;;;###autoload
(eval-after-load 'org
  `(funcall
    ;; The extra quote below is necessary because uncompiled closures
    ;; do not evaluate to themselves. The quote is harmless for
    ;; byte-compiled function objects.
    ',(lambda ()
        (if (version< (org-version) "9.0")
            (with-no-warnings
              (org-add-link-type "elfeed" #'elfeed-link-open)
              (add-hook 'org-store-link-functions #'elfeed-link-store-link))
          (with-no-warnings
            (org-link-set-parameters
             "elfeed"
             :follow #'elfeed-link-open
             :store #'elfeed-link-store-link))))))

(provide 'elfeed-link)

;;; elfeed-link.el ends here
