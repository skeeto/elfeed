;;; extract-urls.el --- Extract non-ignored feed URLs from org file -*- lexical-binding: t; -*-

;; Reads the org file, skips entries tagged :ignore: (including inherited tags),
;; and writes one URL per line to stdout.

(require 'org)
(require 'org-element)

(defun extract-feed-urls (org-file)
  "Extract feed URLs from ORG-FILE, excluding :ignore: tagged entries."
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-mode)
    (let ((urls ()))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let* ((tags (org-element-property :tags hl))
                 (raw-value (org-element-property :raw-value hl)))
            (unless (member "ignore" tags)
              ;; Extract URL from org link in the headline
              (when (string-match "\\[\\[\\(https?://[^]]+\\)\\]" raw-value)
                (push (match-string 1 raw-value) urls))))))
      (nreverse urls))))

(let ((urls (extract-feed-urls (car command-line-args-left))))
  (dolist (url urls)
    (princ (format "%s\n" url))))
