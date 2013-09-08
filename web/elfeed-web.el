;;; elfeed-web.el --- web interface to Elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/elfeed

;;; Commentary:

;; This is a very early work in progress. The long-term goal is to
;; provide a web interface view of the database with optional remote
;; tag updating. An AngularJS client accesses the database over a few
;; RESTful endpoints with JSON for serialization.

;; The IDs provided by RSS and Atom are completely arbitrary. To avoid
;; ugly encoding issues they're normalized into short, unique,
;; alphanumeric codes called webids. Both feeds and entries fall into
;; the same webid namespace so they share a single endpoint.

;; Endpoints:

;; /elfeed/<path>
;;     Serves the static HTML, JS, and CSS content.

;; /elfeed/content/<ref-id>
;;     Serves content from the content database (`elfeed-deref').

;; /elfeed/thing/<webid>
;;     Serve up an elfeed-feed or elfeed-entry in JSON format.

;; /elfeed/search
;;     Accepts a q parameter which is an filter string to be parsed
;;     and handled by `elfeed-search-parse-filter'.

;; /elfeed/update
;;     Accepts a time parameter. If time < `elfeed-db-last-update',
;;     respond with time. Otherwise don't respond until database
;;     updates (long poll).

;;; Code:

(require 'cl)
(require 'json)
(require 'simple-httpd)
(require 'elfeed-db)
(require 'elfeed-search)

(defcustom elfeed-web-enabled nil
  "If true, serve a web interface Elfeed with simple-httpd."
  :group 'elfeed
  :type 'boolean)

(defvar elfeed-web-limit 512
  "Maximum number of entries to serve at once.")

(defvar elfeed-web-data-root (file-name-directory load-file-name)
  "Location of the static Elfeed web data files.")

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
           :contentType  (elfeed-entry-content-type thing)
           :enclosures   (or (mapcar #'car (elfeed-entry-enclosures thing)) [])
           :tags         (or (elfeed-entry-tags thing) [])
           :feed         (elfeed-web-for-json (elfeed-entry-feed thing))))
    (elfeed-feed
     (list :webid  (elfeed-web-make-webid thing)
           :url    (elfeed-feed-url thing)
           :title  (elfeed-feed-title thing)
           :author (elfeed-feed-author thing)))))

(defmacro with-elfeed-web (&rest body)
  "Only execute BODY if `elfeed-web-enabled' is true."
  `(if (not elfeed-web-enabled)
       (progn
         (princ (json-encode '(:error 403)))
         (httpd-send-header t "application/json" 403))
     ,@body))

(defservlet* elfeed/things/:webid application/json ()
  "Return a requested thing (entry or feed)."
  (with-elfeed-web
   (princ (json-encode (elfeed-web-for-json (elfeed-web-lookup webid))))))

(defservlet* elfeed/content/:ref text/html ()
  "Serve content-addressable content at REF."
  (with-elfeed-web
   (let ((content (elfeed-deref (make-elfeed-ref :id ref))))
     (if content
         (princ content)
       (princ (json-encode '(:error 404)))
       (httpd-send-header t "application/json" 404)))))

(defservlet* elfeed/search application/json (q)
  "Perform a search operation with Q and return the results."
  (with-elfeed-web
   (let* ((results (list nil))
          (tail results)
          (filter (elfeed-search-parse-filter q))
          (count elfeed-web-limit))
     (with-elfeed-db-visit (entry feed)
       (when (elfeed-search-filter filter entry feed)
         (setf (cdr tail) (list entry)
               tail (cdr tail))
         (when (< (decf count) 0)
           (elfeed-db-return))))
     (princ (json-encode
             (coerce (mapcar #'elfeed-web-for-json (cdr results)) 'vector))))))

(defvar elfeed-web-waiting ()
  "Clients waiting for an update.")

(defservlet* elfeed/update application/json (time)
  "Return the current :last-update time for the database. If a
time parameter is provided don't respond until the time has
advanced past it (long poll)."
  (let ((update-time (ffloor (elfeed-db-last-update))))
    (if (= update-time (ffloor (string-to-number (or time ""))))
        (push (httpd-discard-buffer) elfeed-web-waiting)
      (princ (json-encode update-time)))))

(defservlet elfeed text/plain (uri-path _ request)
  "Serve static files from `elfeed-web-data-root'."
  (if (not elfeed-web-enabled)
      (insert "Elfeed web interface is disabled.\n"
              "Set `elfeed-web-enabled' to true to enable it.")
    (let ((base "/elfeed/"))
      (if (< (length uri-path) (length base))
          (httpd-redirect t base)
        (let ((path (substring uri-path (1- (length base)))))
          (httpd-serve-root t elfeed-web-data-root path request))))))

(defun elfeed-web-update ()
  "Update waiting clients about database changes."
  (while elfeed-web-waiting
    (let ((proc (pop elfeed-web-waiting)))
      (ignore-errors
        (with-httpd-buffer proc "application/json"
          (princ (json-encode (ffloor (elfeed-db-last-update)))))))))

(add-hook 'elfeed-db-update-hook 'elfeed-web-update)

;;;###autoload
(defun elfeed-web-start ()
  "Start the Elfeed web interface server."
  (interactive)
  (httpd-start)
  (setf elfeed-web-enabled t))

(defun elfeed-web-stop ()
  "Stop the Elfeed web interface server."
  (interactive)
  (setf elfeed-web-enabled nil))

(provide 'elfeed-web)

;;; elfeed-web.el ends here
