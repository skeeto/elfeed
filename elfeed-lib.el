;;; elfeed-lib.el --- misc functions for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

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

(provide 'elfeed-lib)

;;; elfeed-lib.el ends here
