;;; elfeed-web.el --- web interface to Elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'json)
(require 'simple-httpd)
(require 'elfeed-db)
(require 'elfeed-search)

(defvar elfeed-web-webid-map (make-hash-table :test 'equal)
  "Track the mapping between entries and IDs.")

(defun elfeed-web-make-webid (thing)
  "Compute a unique web ID for THING."
  (let* ((thing-id (prin1-to-string (aref thing 1)))
         (hash (base64-encode-string (secure-hash 'sha1 thing-id nil nil t)))
         (webid (substring hash 0 8)))
    (setf (gethash webid elfeed-web-webid-map) thing)
    webid))

(defun elfeed-web-lookup (webid)
  "Lookup a thing by its WEBID."
  (let ((thing (gethash webid elfeed-web-webid-map)))
    (if thing
        thing
      (or (with-elfeed-db-visit (entry _)
            (when (string= webid (elfeed-web-make-webid entry))
              (setf (gethash webid elfeed-web-webid-map)
                    (elfeed-db-return entry))))
          (loop for feed hash-values of elfeed-db-feeds
                when (string= (elfeed-web-make-webid feed) webid)
                return (setf (gethash webid elfeed-web-webid-map) feed))))))

(defun elfeed-web-for-json (thing)
  "Prepare THING for JSON serialization."
  (etypecase thing
    (elfeed-entry
     (list :webid        (elfeed-web-make-webid thing)
           :title        (elfeed-entry-title thing)
           :link         (elfeed-entry-link thing)
           :date         (* 1000 (elfeed-entry-date thing))
           :content      (let ((content (elfeed-entry-content thing)))
                           (and content (elfeed-ref-id content)))
           :content-type (elfeed-entry-content-type thing)
           :enclosures   (or (mapcar #'car (elfeed-entry-enclosures thing)) [])
           :tags         (or (elfeed-entry-tags thing) [])
           :feed-id      (elfeed-web-make-webid (elfeed-entry-feed thing))))
    (elfeed-feed
     (list :webid  (elfeed-web-make-webid thing)
           :url    (elfeed-feed-url thing)
           :title  (elfeed-feed-title thing)
           :author (elfeed-feed-author thing)))))

(defservlet* elfeed/things/:webid application/json ()
  "Return a requested thing (entry or feed)."
  (princ (json-encode (elfeed-web-for-json (elfeed-web-lookup webid)))))

(defservlet* elfeed/content/:ref text/plain ()
  "Serve content-addressable content at REF."
  (let ((content (elfeed-deref (make-elfeed-ref :id ref))))
    (if content
        (princ content)
      (httpd-error t 404 "Content not found."))))

(defservlet* elfeed/search application/json (q)
  "Perform a search operation with Q and return the results."
  (let* ((results (list nil))
         (tail results)
         (filter (elfeed-search-parse-filter q)))
    (with-elfeed-db-visit (entry feed)
      (when (elfeed-search-filter filter entry feed)
        (setf (cdr tail) (list entry)
              tail (cdr tail))))
    (princ (json-encode
            (coerce (mapcar #'elfeed-web-for-json (cdr results)) 'vector)))))

(provide 'elfeed-web)

;;; elfeed-web.el ends here
