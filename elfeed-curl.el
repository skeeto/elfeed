;;; elfeed-curl.el --- curl backend for Elfeed -*- lexical-binding: t; -*-

;;; Comments:

;; An alternative to `url-retrieve' and `url-queue' that fetches URLs
;; using the curl command line program.

;; The API is three functions:

;; * `elfeed-curl-retrieve'
;; * `elfeed-curl-retrieve-synchronously'
;; * `elfeed-curl-enqueue'

;; And has four buffer-local variables for use in callbacks:

;; * `elfeed-curl-headers'
;; * `elfeed-curl-status-code'
;; * `elfeed-curl-error-message'
;; * `elfeed-curl-location'

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

(defvar-local elfeed-curl-location nil
  "Actual URL fetched (after any redirects).")

(defvar elfeed-curl--error-codes
  '((1 . "Unsupported protocol.")
    (2 . "Failed to initialize.")
    (3 . "URL malformed. The syntax was not correct.")
    (4 . "A feature or option that was needed to perform the desired request was not enabled or was explicitly disabled at build-time.")
    (5 . "Couldn't resolve proxy. The given proxy host could not be resolved.")
    (6 . "Couldn't resolve host. The given remote host was not resolved.")
    (7 . "Failed to connect to host.")
    (8 . "FTP weird server reply. The server sent data curl couldn't parse.")
    (9 . "FTP access denied.")
    (11 . "FTP weird PASS reply.")
    (13 . "FTP weird PASV reply.")
    (14 . "FTP weird 227 format.")
    (15 . "FTP can't get host.")
    (17 . "FTP couldn't set binary.")
    (18 . "Partial file. Only a part of the file was transferred.")
    (19 . "FTP couldn't download/access the given file, the RETR (or similar) command failed.")
    (21 . "FTP quote error. A quote command returned error from the server.")
    (22 . "HTTP page not retrieved.")
    (23 . "Write error.")
    (25 . "FTP couldn't STOR file.")
    (26 . "Read error. Various reading problems.")
    (27 . "Out of memory. A memory allocation request failed.")
    (28 . "Operation timeout.")
    (30 . "FTP PORT failed.")
    (31 . "FTP couldn't use REST.")
    (33 . "HTTP range error. The range \"command\" didn't work.")
    (34 . "HTTP post error. Internal post-request generation error.")
    (35 . "SSL connect error. The SSL handshaking failed.")
    (36 . "FTP bad download resume.")
    (37 . "FILE couldn't read file.")
    (38 . "LDAP bind operation failed.")
    (39 . "LDAP search failed.")
    (41 . "Function not found. A required LDAP function was not found.")
    (42 . "Aborted by callback.")
    (43 . "Internal error. A function was called with a bad parameter.")
    (45 . "Interface error. A specified outgoing interface could not be used.")
    (47 . "Too many redirects.")
    (48 . "Unknown option specified to libcurl.")
    (49 . "Malformed telnet option.")
    (51 . "The peer's SSL certificate or SSH MD5 fingerprint was not OK.")
    (52 . "The server didn't reply anything, which here is considered an error.")
    (53 . "SSL crypto engine not found.")
    (54 . "Cannot set SSL crypto engine as default.")
    (55 . "Failed sending network data.")
    (56 . "Failure in receiving network data.")
    (58 . "Problem with the local certificate.")
    (59 . "Couldn't use specified SSL cipher.")
    (60 . "Peer certificate cannot be authenticated with known CA certificates.")
    (61 . "Unrecognized transfer encoding.")
    (62 . "Invalid LDAP URL.")
    (63 . "Maximum file size exceeded.")
    (64 . "Requested FTP SSL level failed.")
    (65 . "Sending the data requires a rewind that failed.")
    (66 . "Failed to initialise SSL Engine.")
    (67 . "The user name, password, or similar was not accepted and curl failed to log in.")
    (68 . "File not found on TFTP server.")
    (69 . "Permission problem on TFTP server.")
    (70 . "Out of disk space on TFTP server.")
    (71 . "Illegal TFTP operation.")
    (72 . "Unknown TFTP transfer ID.")
    (73 . "File already exists (TFTP).")
    (74 . "No such user (TFTP).")
    (75 . "Character conversion failed.")
    (76 . "Character conversion functions required.")
    (77 . "Problem with reading the SSL CA cert (path? access rights?).")
    (78 . "The resource referenced in the URL does not exist.")
    (79 . "An unspecified error occurred during the SSH session.")
    (80 . "Failed to shut down the SSL connection.")
    (82 . "Could not load CRL file, missing or wrong format (added in 7.19.0).")
    (83 . "Issuer check failed (added in 7.19.0).")
    (84 . "The FTP PRET command failed")
    (85 . "RTSP: mismatch of CSeq numbers")
    (86 . "RTSP: mismatch of Session Identifiers")
    (87 . "unable to parse FTP file list")
    (88 . "FTP chunk callback reported error")
    (89 . "No connection available, the session will be queued")
    (90 . "SSL public key does not matched pinned public key")))

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

(defun elfeed-curl--adjust-location (old-url new-url)
  "Return full URL for maybe-relative NEW-URL from full URL old-url."
  (let ((old (url-generic-parse-url old-url))
        (new (url-generic-parse-url new-url)))
    (cond
     ;; Is new URL absolute already?
     ((url-type new) new-url)
     ;; Does it start with //? Append the old protocol.
     ((url-fullness new) (concat (url-type old) new-url))
     ;; Is it a relative path?
     ((not (string-match-p "^/" new-url))
      (let* ((old-dir (or (file-name-directory (url-filename old)) "/"))
             (new-file (concat old-dir new-url)))
        (setf (url-filename old) nil
              (url-target old) nil
              (url-attributes old) nil
              (url-filename old) new-file)
        (url-recreate-url old)))
     ;; Replace the relative part.
     ((progn
        (setf (url-filename old) new-url
              (url-target old) nil
              (url-attributes old) nil)
        (url-recreate-url old))))))

(defun elfeed-curl--parse-http ()
  "Parse HTTP headers, setting the appropriate local variables."
  (setf elfeed-curl-status-code nil)
  (while (null elfeed-curl-status-code)
    (setf (point) (point-min)
          elfeed-curl-status-code (elfeed-curl--parse-response)
          elfeed-curl-headers (elfeed-curl--parse-headers))
    (let ((location (cdr (assoc "location" elfeed-curl-headers))))
      (when (and (>= elfeed-curl-status-code 300)
                 (< elfeed-curl-status-code 400)
                 location)
        (setf elfeed-curl-status-code nil
              elfeed-curl-location
              (elfeed-curl--adjust-location elfeed-curl-location location))))))

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
                            (coding-system-from-name
                             (match-string 1 content-type))))
    (set-buffer-multibyte t)))

(defun elfeed-curl-retrieve-synchronously (url &optional headers)
  "Retrieve the contents for URL and return a new buffer with them.
HEADERS is an alist of additional headers to add to the HTTP request."
  (let ((is-http (elfeed-curl--url-is-http url)))
    (with-current-buffer (generate-new-buffer (format "*curl %s*" url))
      (setf elfeed-curl-location url)
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
          ;; process abnormal exit
          (if (string-match "exited abnormally with code \\([0-9]+\\)" status)
              (let* ((code (string-to-number (match-string 1 status)))
                     (message (cdr (assoc code elfeed-curl--error-codes))))
                (setf elfeed-curl-error-message
                      (format "(%d) %s"
                              code (or message "Unknown curl error!")))
                (funcall cb nil))
            (setf elfeed-curl-error-message status)
            (funcall cb nil))
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
        (setf elfeed-curl-location url)
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
