;;; elfeed-lib-tests.el --- library tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'elfeed-lib)

(ert-deftest elfeed-goto-line ()
  (with-temp-buffer
    (insert "a\nbb\nccc\ndddd\n")
    (elfeed-goto-line 2)
    (should (looking-at "bb"))
    (elfeed-goto-line 4)
    (should (looking-at "dddd"))))

(ert-deftest elfeed-kill-line ()
  (with-temp-buffer
    (insert "a\nbb\nccc\ndddd\n")
    (elfeed-goto-line 3)
    (elfeed-kill-line)
    (should (equal (buffer-string) "a\nbb\n\ndddd\n"))))

(ert-deftest elfeed-time-duration ()
  (mapc (lambda (test)
          (let ((expect (eval (car test)))
                 (check (eval (cadr test))))
            (should (= expect check))))
    '(((* 1.0 7 24 60 60)      (elfeed-time-duration "1 week ago"))
      ((* 3.0 365.25 24 60 60) (elfeed-time-duration "3 years old"))
      ((* 1.0 24 60 60)        (elfeed-time-duration "1-day"))
      ((* 1.0 60 60)           (elfeed-time-duration "1hour")))))

(ert-deftest elfeed-format-column ()
  (mapc (lambda (test)
          (let ((check (eval (car test)))
                 (expect (cadr test)))
            (should (string= expect check))))
    '(((elfeed-format-column "foo" 10 :right) "       foo")
      ((elfeed-format-column "foo" 10 :left) "foo       ")
      ((elfeed-format-column "foo" 2 :left) "fo")
      ((elfeed-format-column "foo" 2 :right) "fo")
      ((elfeed-format-column "foo" 0) "")
      ((elfeed-format-column "foo" -1) ""))))

(ert-deftest elfeed-clamp ()
  (mapc (lambda (test)
          (let ((check (eval (car test)))
                 (expect (cadr test)))
            (should (= expect check))))
    '(((elfeed-clamp  0 3 4) 3)
      ((elfeed-clamp  2 9 4) 4)
      ((elfeed-clamp  2 0 4) 2)
      ((elfeed-clamp -6 3 0) 0))))

(ert-deftest elfeed-valid-regexp-p ()
  (mapc (lambda (test)
          (let ((valid-p (car test))
                 (check (cadr test)))
            (if valid-p
              (should (elfeed-valid-regexp-p check))
              (should-not (elfeed-valid-regexp-p check)))))
    '((t "")
      (t "[abc]\\.")
      (nil "\\")
      (nil "[")
      (nil :foo))))

(ert-deftest elfeed-looks-like-url-p ()
  (should (elfeed-looks-like-url-p "http://nullprogram.com/"))
  (should (elfeed-looks-like-url-p "https://example.com/"))
  (should-not (elfeed-looks-like-url-p "example.com"))
  (should-not (elfeed-looks-like-url-p "foo bar"))
  (should-not (elfeed-looks-like-url-p nil)))

(ert-deftest elfeed-cleanup ()
  (mapc (lambda (test)
          (let ((expect (car test))
                 (check (eval (cadr test))))
            (should (string= expect check))))
    '(("foo  bar" (elfeed-cleanup "  foo  bar\n"))
       ("foo bar" (elfeed-cleanup "foo\nbar")))))

(ert-deftest elfeed-float-time ()
  (cl-macrolet ((test (time seconds)
                   `(should (= (elfeed-float-time ,time) ,seconds))))
    (test "1985-03-24T03:23:42Z"          480482622.0)
    (test "Mon,  5 May 1986 15:16:09 GMT" 515690169.0)
    (test "2015-02-20" 1424390400.0)
    (test "20150220" 1424390400.0)
    (test "2015-02" 1422748800.0)))

(ert-deftest elfeed-xml-parse-region ()
  (with-temp-buffer
    (insert
     (encode-coding-string
      "<?xml version=\"1.0\" encoding=\"windows-1251\"?>
<title>Тест</title>"
      'windows-1251))
    (let ((xml (elfeed-xml-parse-region)))
      (should (string= "Тест" (nth 2 (nth 0 xml))))))
  (with-temp-buffer
    (insert
     (encode-coding-string
      "<?xml version='1.0' encoding='windows-1251'?>
<title>Тест</title>"
      'windows-1251))
    (let ((xml (elfeed-xml-parse-region)))
      (should (string= "Тест" (nth 2 (nth 0 xml))))))
  (with-temp-buffer
    (insert
     (concat
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rss>"
      (mapconcat (lambda (i) " ") (number-sequence 1 100000) "")
      "</rss>"))
    (elfeed-xml-parse-region)))

(ert-deftest elfeed-directory-empty-p ()
  (let ((empty (make-temp-file "empty" t))
        (full (make-temp-file "full" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "foo" full))
          (should (elfeed-directory-empty-p empty))
          (should-not (elfeed-directory-empty-p full)))
      (delete-directory empty :recursive)
      (delete-directory full  :recursive))))

(ert-deftest elfeed-slurp-spit ()
  (let ((file (make-temp-file "spit"))
        (data (string 40 400 4000 40000)))
    (unwind-protect
        (progn
          (elfeed-spit file data)
          (should (string= (elfeed-slurp file) data))
          (elfeed-spit file data :append t)
          (should (string= (elfeed-slurp file) (concat data data))))
      (delete-file file))))

(ert-deftest elfeed-keyword->symbol ()
  (should (eq (elfeed-keyword->symbol :foo) 'foo))
  (should (eq (elfeed-keyword->symbol 'foo) 'foo)))

(ert-deftest elfeed-resize-vector ()
  (mapc (lambda (test)
          (let ((expect (car test))
                 (check (eval (cadr test))))
            (should (equal expect check))))
    '(([nil nil] (elfeed-resize-vector [] 2))
       ([1 2] (elfeed-resize-vector [1 2 3 4] 2))
       ([9 8 7 nil] (elfeed-resize-vector [9 8 7] 4)))))

(ert-deftest elfeed-readable-p ()
  (should (elfeed-readable-p t))
  (should (elfeed-readable-p nil))
  (should-not (elfeed-readable-p (current-buffer)))
  (should (elfeed-readable-p 101))
  (should-not (elfeed-readable-p (make-marker)))
  (should (elfeed-readable-p "foobar"))
  (should (elfeed-readable-p (make-hash-table)))
  (should-not (elfeed-readable-p (symbol-function '+))))

(ert-deftest elfeed-move-to-first-empty-line ()
  (with-temp-buffer
    (insert "aaaaa\nbbbb\n\ncccccc")
    (elfeed-move-to-first-empty-line)
    (should (= (point) 12)))
  (with-temp-buffer
    (insert "aaaaa\nbbbb\ncccccc")
    (setf (point) 5)
    (elfeed-move-to-first-empty-line)
    (should (= (point) 5))))

(provide 'elfeed-lib-tests)

;;; elfeed-lib-tests.el ends here
