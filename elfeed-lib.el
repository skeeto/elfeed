;;; elfeed-lib.el --- misc functions for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; These are general functions that aren't specific to web feeds. It's
;; a library of useful functions to Elfeed.

;;; Code:

(require 'cl)
(require 'time-date)
(require 'url-parse)

(defun elfeed-expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda () (interactive) (apply function args)))

(defun elfeed-goto-line (n)
  "Like `goto-line' but for non-interactive use."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun elfeed-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defmacro elfeed-save-excursion (&rest body)
  "Like `save-excursion', but by line/column instead of point."
  (declare (indent defun))
  `(let ((line (line-number-at-pos))
         (column (current-column)))
     (unwind-protect
         (progn ,@body)
       (elfeed-goto-line line)
       (move-to-column column))))

(defun elfeed-kill-line ()
  "Clear out the current line without touching anything else."
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (delete-region start (point))))

(defun elfeed-time-duration (time)
  "Turn a time expression into a number of seconds. Uses
`timer-duration' but allows a bit more flair."
  (if (numberp time)
      time
    (timer-duration (replace-regexp-in-string "\\(ago\\|old\\|-\\)" "" time))))

(defun elfeed-looks-like-url-p (string)
  "Return true if STRING looks like it could be a URL."
  (and (stringp string)
       (not (string-match-p "[ \n\t\r]" string))
       (not (null (url-type (url-generic-parse-url string))))))

(defun elfeed-format-column (string width &optional align)
  "Return STRING truncated or padded to WIDTH following ALIGNment.
Align should be a keyword :left or :right."
  (if (<= width 0)
      ""
    (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") width width)
            string)))

(defun elfeed-clamp (min value max)
  "Clamp a value between two values."
  (min max (max min value)))

(defun elfeed-valid-regexp-p (regexp)
  "Return t if REGEXP is a valid REGEXP."
  (ignore-errors
    (prog1 t
      (string-match-p regexp ""))))

(defun elfeed-cleanup (name)
  "Trim trailing and leading spaces and collapse multiple spaces."
  (let ((trim (replace-regexp-in-string "[\n\t]+" " " (or name ""))))
    (replace-regexp-in-string "^ +\\| +$" "" trim)))

(defun elfeed-float-time (&optional date)
  "Like `float-time' but accept anything reasonable for DATE,
defaulting to the current time if DATE could not be parsed. Date
is allowed to be relative to now (`elfeed-time-duration')."
  (typecase date
    (string
     (let ((duration (elfeed-time-duration date)))
       (if duration
           (- (float-time) duration)
         (let ((time (ignore-errors (date-to-time date))))
           (if (equal time '(14445 17280)) ; date-to-time silently failed
               (float-time)
             (float-time time))))))
    (integer date)
    (list (float-time date))
    (t (float-time))))

(defun elfeed-xml-parse-region (&optional beg end buffer parse-dtd parse-ns)
  "Decode (if needed) and parse XML file. Uses coding system from
XML encoding declaration."
  (let ((coding-system nil))
    (progn
      (unless beg (setq beg (point-min)))
      (unless end (setq end (point-max)))
      (goto-char beg)
      (if (re-search-forward "<\\?xml.*?encoding=\"\\([^\"]+\\)\".*?\\?>" nil t)
          (setq coding-system
                (ignore-errors (check-coding-system
                                (intern (downcase (match-string 1)))))))
      (when coding-system
        (setq end (+ beg
                     (decode-coding-region beg end coding-system))))
      (goto-char beg)))
    (xml-parse-region beg end buffer parse-dtd parse-ns))

(provide 'elfeed-lib)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; elfeed-lib.el ends here
