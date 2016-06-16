;;; elfeed-curl.el --- curl backend for Elfeed -*- lexical-binding: t; -*-

;;; Comments:

;; An alternative to `url-retrieve' and `url-queue' that fetches URLs
;; using the curl command line program.

;; The API is three functions:

;; * `elfeed-curl-retrieve'
;; * `elfeed-curl-retrieve-synchronously'
;; * `elfeed-curl-enqueue'

;;; Code:

(require 'url)
(require 'cl-lib)

(defcustom elfeed-curl-program-name "curl"
  "Name/path by which to invoke the curl program."
  :group 'elfeed)

(defcustom elfeed-curl-max-connections 16
  "Maximum number of concurrent fetches."
  :group 'elfeed)

(defcustom elfeed-curl-timeout 30
  "Maximum number of seconds a fetch is allowed to take once started."
  :group 'elfeed)

(defvar elfeed-curl-queue ()
  "List of pending curl requests.")

(defvar elfeed-curl-queue-active 0
  "Number of concurrent requests currently active.")

(defvar-local elfeed-curl-headers nil
  "Alist of HTTP response headers.")

(defvar-local elfeed-curl-status-code nil
  "Numeric HTTP response code, nil for non-HTTP protocols.")

(defvar-local elfeed-curl-error-message nil
  "Human-friendly message describing the error.")

(defun elfeed-curl--parse-headers ()
  "Parse the HTTP response headers, setting `elfeed-curl-headers'."
  (prog1
      (cl-loop until (looking-at "\r?\n")
               do (re-search-forward "\\([^:]+\\): +\\([^\r\n]+\\)")
               collect (cons (downcase (match-string 1)) (match-string 2))
               do (forward-line))
    (forward-line 1)
    (delete-region (point-min) (point))))

(defun elfeed-curl--parse-response ()
  "Parse the HTTP response status, setting `elfeed-curl-status-code'."
  (re-search-forward "HTTP/[.0-9]+ +\\([0-9]+\\)")
  (forward-line 1)
  (string-to-number (match-string 1)))

(defun elfeed-curl--parse-http ()
  "Parse HTTP headers, setting the appropriate local variables."
  (setf elfeed-curl-status-code nil)
  (while (null elfeed-curl-status-code)
    (setf (point) (point-min)
          elfeed-curl-status-code (elfeed-curl--parse-response)
          elfeed-curl-headers (elfeed-curl--parse-headers))
    (when (and (>= elfeed-curl-status-code 300)
               (< elfeed-curl-status-code 400)
               (assoc "location" elfeed-curl-headers))
      (setf elfeed-curl-status-code nil))))

(defun elfeed-curl--url-is-http (url)
  "Return non-nil if URL it HTTP or HTTPS."
  (let ((type (url-type (url-generic-parse-url url))))
    (not (null (member type '("http" "https"))))))

(defun elfeed-curl--args (is-http url headers)
  "Build an argument list for curl."
  (let ((args ()))
    (push "--compressed" args)
    (push "--http1.1" args) ; too many broken HTTP/2 servers
    (push  "-sL" args)
    (push "-m" args)
    (push (format "%s" elfeed-curl-timeout) args)
    (when is-http
      (push "-D" args)
      (push "-" args))
    (dolist (header headers)
      (cl-destructuring-bind (key . value) header
        (if (equal (downcase key) "user-agent")
            (progn
              (push "-A" args)
              (push value args))
          (push "-H" args)
          (push (format "%s: %s" key value) args))))
    (nreverse (cons url args))))

(defun elfeed-curl--decode ()
  "Try to decode the buffer based on the headers."
  (let ((content-type (cdr (assoc "Content-Type" elfeed-curl-headers))))
    (when (and content-type
               (string-match "charset=\\(.+\\)" content-type))
      (decode-coding-region (point-min) (point-max)
                            (coding-system-from-name (match-string 1 content-type))))
    (set-buffer-multibyte t)))

(defun elfeed-curl-retrieve-synchronously (url &optional headers)
  "Retrieve the contents for URL and return a new buffer with them.
HEADERS is an alist of additional headers to add to the HTTP request."
  (let ((is-http (elfeed-curl--url-is-http url)))
    (with-current-buffer (generate-new-buffer (format "*curl %s*" url))
      (buffer-disable-undo)
      (set-buffer-multibyte nil)
      (let ((args (elfeed-curl--args is-http url headers))
            (coding-system-for-read 'raw-text))
        (apply #'call-process elfeed-curl-program-name nil t nil args))
      (if is-http
          (elfeed-curl--parse-http))
      (setf (point) (point-min))
      (elfeed-curl--decode)
      (current-buffer))))

(defun elfeed-curl--cb-wrapper (process status)
  "Adapts a elfeed-curl callback into a process sentinel."
  (let ((buffer (process-buffer process))
        (cb (process-get process :cb))
        (is-http (process-get process :is-http)))
    (with-current-buffer buffer
      (if (not (equal status "finished\n"))
          (funcall cb nil)
        (when is-http
          (condition-case _
              (elfeed-curl--parse-http)
            (error (setf elfeed-curl-error-message
                         "Unable to parse response.")
                   (funcall cb nil))))
        (setf (point) (point-min))
        (elfeed-curl--decode)
        (if (and (>= elfeed-curl-status-code 400)
                 (<= elfeed-curl-status-code 599))
            (progn
              (setf elfeed-curl-error-message
                    (format "HTTP %d" elfeed-curl-status-code))
              (funcall cb nil))
          (funcall cb t))))))

(defun elfeed-curl-retrieve (url cb &optional headers)
  "Retrieve URL contents asynchronously, calling CB with one status argument.
The destination buffer is set at the current buffer for the
callback, which is responsible for killing the buffer. HEADERS is
an alist of additional headers to add to the HTTP request. This
function returns the destination buffer."
  (let* ((buffer (generate-new-buffer (format "*curl %s*" url)))
         (is-http (elfeed-curl--url-is-http url))
         (args (elfeed-curl--args is-http url headers)))
    (prog1 buffer
      (buffer-disable-undo buffer)
      (with-current-buffer buffer
        (set-buffer-multibyte nil))
      (let* ((coding-system-for-read 'raw-text)
             (process (apply #'start-process "elfeed-curl" buffer
                             elfeed-curl-program-name args)))
        (setf (process-sentinel process) #'elfeed-curl--cb-wrapper
              (process-get process :cb) cb
              (process-get process :is-http) is-http)))))

(defun elfeed-curl--run-queue ()
  "Possibly fire off some new requests."
  (while (and (< elfeed-curl-queue-active elfeed-curl-max-connections)
              (> (length elfeed-curl-queue) 0))
    (cl-destructuring-bind (url cb headers) (pop elfeed-curl-queue)
      (cl-incf elfeed-curl-queue-active)
      (elfeed-curl-retrieve url
                            (lambda (status)
                              (cl-decf elfeed-curl-queue-active)
                              (elfeed-curl--run-queue)
                              (funcall cb status))
                            headers))))

(defun elfeed-curl-enqueue (url cb &optional headers)
  "Just like `elfeed-curl-retrieve', but restricts concurrent fetches."
  (let ((entry (list url cb headers)))
    (setf elfeed-curl-queue (nconc elfeed-curl-queue (list entry)))
    (elfeed-curl--run-queue)))

(provide 'elfeed-curl)

;;; elfeed-curl.el ends here
