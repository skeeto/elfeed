;;; elfeed-db-tests.el --- database tests -*- lexical-binding: t; -*-

(require 'cl)
(require 'ert)
(require 'url-parse)
(require 'elfeed-db)
(require 'elfeed-lib)

(defvar elfeed-test-random-state [cl-random-state-tag -1 30 267466518]
  "Use the same random state for each run.")

(defun elfeed-random* (x)
  "Generate a random number from `elfeed-test-random-state'."
  (random* x elfeed-test-random-state))

(defun elfeed-test-generate-letter (&optional multibyte)
  "Generate a single character from a-z or unicode."
  (cl-flet ((control-p (char)
              (or (<= char #x001F) (and (>= char #x007F) (<= char #x009F)))))
    (if multibyte
        (loop for char = (elfeed-random* (1+ #x10FF))
              unless (control-p char) return char)
      (+ ?a (elfeed-random* 26)))))

(defun* elfeed-test-random (n &optional (variance 1.0))
  "Generate a random integer around N, minimum of 1."
  (max 1 (floor (+ n (- (elfeed-random* (* 1.0 variance n))
                        (* variance 0.5 n))))))

(defun* elfeed-test-generate-word (&optional multibyte (length 6))
  "Generate a word around LENGTH letters long."
  (let ((variance 1.0))
    (apply #'string
           (loop repeat (elfeed-test-random length)
                 collect (elfeed-test-generate-letter multibyte)))))

(defun* elfeed-test-generate-title (&optional multibyte (length 8))
  "Generate a title around LENGTH words long, capitalized."
  (mapconcat #'identity
             (loop repeat (elfeed-test-random length)
                   collect (elfeed-test-generate-word multibyte) into words
                   finally (return (cons (capitalize (car words)) (cdr words))))
             " "))

(defun elfeed-test-generate-url ()
  "Generate a random URL."
  (let* ((tlds '(".com" ".net" ".org"))
         (tld (nth (elfeed-random* (length tlds)) tlds))
         (path (downcase (elfeed-test-generate-title nil 3))))
    (url-recreate-url
     (url-parse-make-urlobj
      "http" nil nil
      (concat (elfeed-test-generate-word nil 10) tld)
      nil
      (concat "/" (replace-regexp-in-string " " "/" path))
      nil nil :full))))

(defmacro with-elfeed-test (&rest body)
  "Run BODY with a fresh, empty database that will be destroyed on exit."
  (declare (indent defun))
  `(let* ((elfeed-db nil)
          (elfeed-db-feeds nil)
          (elfeed-db-entries nil)
          (elfeed-db-index nil)
          (elfeed-feeds nil)
          (temp-dir (make-temp-file "elfeed-test-" t))
          (elfeed-db-directory temp-dir)
          (elfeed-new-entry-hook nil)
          (elfeed-db-update-hook nil)
          (elfeed-initial-tags '(unread)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir :recursive))))

(defun elfeed-test-generate-feed ()
  "Generate a random feed. Warning: run this in `with-elfeed-test'."
  (let* ((url (elfeed-test-generate-url))
         (id url)
         (feed (elfeed-db-get-feed id)))
    (prog1 feed
      (push url elfeed-feeds)
      (setf (elfeed-feed-title feed) (elfeed-test-generate-title))
      (setf (elfeed-feed-url feed) url))))

(defun* elfeed-test-generate-date (&optional (within "1 year"))
  "Generate an epoch time within WITHIN time before now."
  (let* ((duration (elfeed-time-duration within))
         (min-time (- (float-time) duration)))
    (+ min-time (elfeed-random* duration))))

(defun* elfeed-test-generate-entry (feed &optional (within "1 year"))
  "Generate a random entry. Warning: run this in `with-elfeed-test'."
  (let* ((feed-id (elfeed-feed-id feed))
         (link (elfeed-test-generate-url)))
    (make-elfeed-entry
     :id (cons feed-id link)
     :title (elfeed-test-generate-title)
     :link link
     :date (elfeed-test-generate-date within)
     :tags (list 'unread)
     :feed-id feed-id)))

(ert-deftest elfeed-db-size ()
  (let ((count 143))
    (with-elfeed-test
      (let ((feed (elfeed-test-generate-feed)))
        (elfeed-db-add
         (loop repeat count collect (elfeed-test-generate-entry feed))))
      (should (= (elfeed-db-size) count)))))

(ert-deftest elfeed-db-merge ()
  (with-elfeed-test
    (let* ((feed (elfeed-test-generate-feed))
           (entry (elfeed-test-generate-entry feed))
           (update (copy-seq entry)))
      (should (eq (elfeed-entry-merge entry update) nil))
      (setf (elfeed-entry-title update) (elfeed-test-generate-title))
      (should (eq (elfeed-entry-merge entry update) t)))))

(ert-deftest elfeed-db-tag ()
  (with-elfeed-test
    (let* ((feed (elfeed-test-generate-feed))
           (entry (elfeed-test-generate-entry feed))
           (tags (elfeed-normalize-tags '(foo bar baz))))
      (apply #'elfeed-tag entry tags)
      (elfeed-untag entry 'unread)
      (should (equal (elfeed-entry-tags entry) tags))
      (should (elfeed-tagged-p 'foo entry))
      (should (elfeed-tagged-p 'bar entry))
      (should (elfeed-tagged-p 'baz entry))
      (should-not (elfeed-tagged-p 'unread entry)))))

(ert-deftest elfeed-db-visit ()
  (with-elfeed-test
    (loop for feed in (loop repeat 8 collect (elfeed-test-generate-feed))
          do (elfeed-db-add
              (loop repeat 10 collect (elfeed-test-generate-entry feed))))
    (let ((entries nil)
          (feeds nil))
      (with-elfeed-db-visit (entry feed)
        (push (elfeed-entry-date entry) entries)
        (pushnew feed feeds :test #'equal))
      ;; All entries should have appeared.
      (should (= (length entries) 80))
      ;; All feeds should have appeared.
      (should (= (length feeds) 8))
      ;; All entries should have appeared in date order
      (should (equal (sort (copy-seq entries) #'<) entries))
      entries)))

(ert-deftest elfeed-db-dates ()
  (with-elfeed-test
    (let* ((feed (elfeed-test-generate-feed))
           (entries (loop repeat 100 collect
                          (elfeed-test-generate-entry feed)))
           (updated-p nil))
      (elfeed-db-add entries)
      (add-hook 'elfeed-new-entry-hook
                (apply-partially #'error "No new entries expected!"))
      (add-hook 'elfeed-db-update-hook
                (lambda () (setf updated-p t)))
      (elfeed-db-add
       (loop for entry in entries
             for update = (copy-seq entry)
             do (setf (elfeed-entry-date update) (elfeed-test-generate-date))
             collect update))
      (should updated-p)
      (let ((collected nil)
            (sorted nil))
        (with-elfeed-db-visit (entry _)
          (push (elfeed-entry-date entry) collected))
        (setf sorted (sort (copy-seq collected) #'<))
        (should (equal collected sorted))))))

(ert-deftest elfeed-ref ()
  (with-elfeed-test
    (let* ((content (loop repeat 25 collect (elfeed-test-generate-title t)))
           (refs (mapcar #'elfeed-ref content))
           (derefs (mapcar #'elfeed-deref refs)))
      (should (equal content derefs)))
    (let ((string "naïveté"))
      (should (string= string (elfeed-deref (elfeed-ref string)))))))

(provide 'elfeed-db-tests)

;;; elfeed-db-tests.el ends here
