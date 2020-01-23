;;; elfeed-db-tests.el --- database tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'url-parse)
(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-lib)
(require 'jka-compr)

(require 'elfeed-tests)

(ert-deftest elfeed-db-size ()
  (let ((count 143))
    (with-elfeed-test
     (let ((feed (elfeed-test-generate-feed)))
       (elfeed-db-add
        (cl-loop repeat count collect (elfeed-test-generate-entry feed))))
     (should (= (elfeed-db-size) count)))))

(ert-deftest elfeed-db-merge ()
  (with-elfeed-test
    (let* ((feed (elfeed-test-generate-feed))
           (entry (elfeed-test-generate-entry feed))
           (update (copy-sequence entry)))
      (should (eq (elfeed-entry-merge entry update) nil))
      (setf (elfeed-entry-title update) (elfeed-test-generate-title))
      (should (eq (elfeed-entry-merge entry update) t)))
    (let ((a (elfeed-entry--create :tags '(a b c) :meta '(:a 1 :b 2)))
          (b (elfeed-entry--create :tags '(c d) :meta '(:b 3 :c 4))))
      (elfeed-entry-merge a b)
      (should (equal (elfeed-entry-tags a) '(a b c)))
      (should (eql (plist-get (elfeed-entry-meta a) :a) 1))
      (should (eql (plist-get (elfeed-entry-meta a) :b) 3))
      (should (eql (plist-get (elfeed-entry-meta a) :c) 4)))))

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
   (cl-loop for feed in (cl-loop repeat 8 collect (elfeed-test-generate-feed))
            do (elfeed-db-add
                (cl-loop repeat 10 collect (elfeed-test-generate-entry feed))))
   (let ((entries nil)
         (feeds nil))
     (with-elfeed-db-visit (entry feed)
       (push (elfeed-entry-date entry) entries)
       (cl-pushnew feed feeds :test #'equal))
     ;; All entries should have appeared.
     (should (= (length entries) 80))
     ;; All feeds should have appeared.
     (should (= (length feeds) 8))
     ;; All entries should have appeared in date order
     (should (equal (sort (copy-sequence entries) #'<) entries))
     entries)))

(ert-deftest elfeed-db-dates ()
  (with-elfeed-test
   (let* ((feed (elfeed-test-generate-feed))
          (entries (cl-loop repeat 100 collect
                            (elfeed-test-generate-entry feed)))
          (updated-p nil))
     (elfeed-db-add entries)
     (add-hook 'elfeed-new-entry-hook
               (apply-partially #'error "No new entries expected!"))
     (add-hook 'elfeed-db-update-hook
               (lambda () (setf updated-p t)))
     (elfeed-db-add
      (cl-loop for entry in entries
               for update = (copy-sequence entry)
               do (setf (elfeed-entry-date update) (elfeed-test-generate-date))
               collect update))
     (should updated-p)
     (let ((collected nil)
           (sorted nil))
       (with-elfeed-db-visit (entry _)
         (push (elfeed-entry-date entry) collected))
       (setf sorted (sort (copy-sequence collected) #'<))
       (should (equal collected sorted))))))

(ert-deftest elfeed-ref ()
  (with-elfeed-test
   (let* ((content (cl-loop repeat 25 collect (elfeed-test-generate-title t)))
          (refs (mapcar #'elfeed-ref content))
          (derefs (mapcar #'elfeed-deref refs)))
     (should (equal content derefs)))
   (let ((string "naïveté"))
     (should (string= string (elfeed-deref (elfeed-ref string)))))))

(ert-deftest elfeed-ref-pack ()
  (catch 'test-abort
    (with-elfeed-test
     (let ((jka-compr-verbose nil)
           (matcher "^[a-z0-9]\\{2\\}$")
           (feed (elfeed-test-generate-feed))
           (data (expand-file-name "data" elfeed-db-directory)))
       (unless (elfeed-gzip-supported-p)
         (message "warning: gzip auto-compression unsupported, skipping")
         (throw 'test-abort nil))
       (cl-flet ((make-entries (n)
                   (cl-loop repeat n
                            for entry = (elfeed-test-generate-entry feed)
                            do (setf (elfeed-entry-title entry)
                                     (elfeed-test-generate-title :multibyte))
                            do (setf (elfeed-entry-content entry)
                                     (elfeed-entry-title entry))
                            collect entry)))
         (let ((entries-a (make-entries 20))
               (entries-b (make-entries 20)))
           (elfeed-db-add entries-a)
           (should (directory-files data nil matcher))
           (elfeed-db-pack)
           (elfeed-db-add entries-b)
           (elfeed-db-pack)
           (elfeed-db-gc)
           (should-not (directory-files data nil matcher))
           (dolist (entry (append entries-a entries-b))
             (let ((title (elfeed-entry-title entry))
                   (content (elfeed-deref (elfeed-entry-content entry))))
               (should (string= title content))))))))))

(ert-deftest elfeed-db-meta ()
  (with-elfeed-test
   (let* ((feed (elfeed-db-get-feed (elfeed-test-generate-url)))
          (entry (elfeed-test-generate-entry feed)))
     (should (null (elfeed-meta feed :status)))
     (should (null (elfeed-meta entry :rating)))
     (should (= (elfeed-meta entry :errors 10) 10))
     (setf (elfeed-meta feed :status) 'down
           (elfeed-meta entry :rating) 4)
     (cl-incf (elfeed-meta entry :errors 0))
     (should (equal 'down (elfeed-meta feed :status)))
     (should (equal 4 (elfeed-meta entry :rating)))
     (should (= (elfeed-meta entry :errors) 1))
     (should-error (setf (elfeed-meta entry :rating) (current-buffer))))))

(ert-deftest elfeed-db-feed-entries ()
  "Test `elfeed-feed-entries'."
  (with-elfeed-test
    (cl-flet ((tsort (x) (sort (mapcar #'elfeed-entry-title x) #'string<)))
      (let* ((feed-a (elfeed-test-generate-feed))
             (feed-a-entries
              (cl-loop repeat 10 collect (elfeed-test-generate-entry feed-a)))
             (feed-b (elfeed-test-generate-feed))
             (feed-b-id (elfeed-feed-id feed-b))
             (feed-b-entries
              (cl-loop repeat 10 collect (elfeed-test-generate-entry feed-b))))
        (elfeed-db-add feed-a-entries)
        (elfeed-db-add feed-b-entries)
        ;; Fetch the entries using `elfeed-feed-entries'
        (should (equal (tsort (elfeed-feed-entries feed-a))
                       (tsort feed-a-entries)))
        (should (equal (tsort (elfeed-feed-entries feed-b-id))
                       (tsort feed-b-entries)))))))

(provide 'elfeed-db-tests)

;;; elfeed-db-tests.el ends here
