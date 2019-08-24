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
  (should (= (elfeed-time-duration "1 week ago")  (* 1.0 7 24 60 60)))
  (should (= (elfeed-time-duration "3 years old") (* 3.0 365.25 24 60 60)))
  (should (= (elfeed-time-duration "1-day")       (* 1.0 24 60 60)))
  (should (= (elfeed-time-duration "1hour")       (* 1.0 60 60))))

(ert-deftest elfeed-time-duration-absolute ()
  ;; fixed time for testing: assume U.S. eastern
  (let ((now (float-time (encode-time 0 20 13 24 6 2019 (* -1 4 60 60)))))
    ;; "2019-06-24T13:20:00-04:00" is "2019-06-24T17:20:00Z" so 17h 20mins is
    ;; the time difference:
    (should (= (+ (* 17 60 60) (* 20 60))
               (elfeed-time-duration "2019-06-24" now)))
    (should (= (* 10 60)
               (elfeed-time-duration "2019-06-24T17:10" now)))
    (should (= (* 10 60)
               (elfeed-time-duration "2019-06-24T17:10:00" now)))
    (should (= (+ (* 9 60) 30)
               (elfeed-time-duration "2019-06-24T17:10:30" now)))
    (should (= (+ (* 9 60) 30)
               (elfeed-time-duration "2019-06-24T17:10:30Z" now)))
    (should (= (+ (* 9 60) 30)
               (elfeed-time-duration "2019-06-24T17:10:30+00:00" now)))
    (should (= (+ (* 9 60) 30)
               (elfeed-time-duration "20190624T17:10:30+00:00" now)))))

(ert-deftest elfeed-format-column ()
  (should (string= (elfeed-format-column "foo" 10 :right) "       foo"))
  (should (string= (elfeed-format-column "foo" 10 :left)  "foo       "))
  (should (string= (elfeed-format-column "foo" 2  :left)  "fo"))
  (should (string= (elfeed-format-column "foo" 2  :right) "fo"))
  (should (string= (elfeed-format-column "foo" 0)  ""))
  (should (string= (elfeed-format-column "foo" -1) "")))

(ert-deftest elfeed-clamp ()
  (should (= (elfeed-clamp  0 3 4) 3))
  (should (= (elfeed-clamp  2 9 4) 4))
  (should (= (elfeed-clamp  2 0 4) 2))
  (should (= (elfeed-clamp -6 3 0) 0)))

(ert-deftest elfeed-valid-regexp-p ()
  (should (elfeed-valid-regexp-p ""))
  (should (elfeed-valid-regexp-p "[abc]\\."))
  (should-not (elfeed-valid-regexp-p "\\"))
  (should-not (elfeed-valid-regexp-p "["))
  (should-not (elfeed-valid-regexp-p :foo)))

(ert-deftest elfeed-looks-like-url-p ()
  (should (elfeed-looks-like-url-p "http://nullprogram.com/"))
  (should (elfeed-looks-like-url-p "https://example.com/"))
  (should-not (elfeed-looks-like-url-p "example.com"))
  (should-not (elfeed-looks-like-url-p "foo bar"))
  (should-not (elfeed-looks-like-url-p nil)))

(ert-deftest elfeed-cleanup ()
  (should (string= (elfeed-cleanup "  foo  bar\n") "foo bar"))
  (should (string= (elfeed-cleanup "foo\nbar") "foo bar")))

(ert-deftest elfeed-float-time ()
  (cl-macrolet ((test (time seconds)
                   `(should (= (elfeed-float-time ,time) ,seconds))))
    (test "1985-03-24"                    480470400.0)
    (test "1985-03-24T03:23:42Z"          480482622.0)
    (test "Mon,  5 May 1986 15:16:09 GMT" 515690169.0)
    (test "2015-02-20" 1424390400.0)
    (test "20150220" 1424390400.0)
    (test "2015-02" 1422748800.0)
    (should (null (elfeed-float-time "notadate")))))

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
      (mapconcat (lambda (_) " ") (number-sequence 1 100000) "")
      "</rss>"))
    (elfeed-xml-parse-region))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "<?xml version='1.0' encoding='gb2312'?>"
            "<x>\xb0\xd9\xb6\xc8\xbf\xc6\xbc\xbc"
            "\xbd\xb9\xb5\xe3\xd0\xc2\xce\xc5</x>")
    (should (equal (elfeed-xml-parse-region) '((x nil "百度科技焦点新闻"))))))

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
  (should (equal [nil nil] (elfeed-resize-vector [] 2)))
  (should (equal [1 2] (elfeed-resize-vector [1 2 3 4] 2)))
  (should (equal [9 8 7 nil] (elfeed-resize-vector [9 8 7] 4))))

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

(ert-deftest elfeed-update-location ()
  (cl-macrolet ((t (o n e)
                   `(should (equal (elfeed-update-location ,o ,n) ,e))))
    (t "http://foo.example/" "/foo" "http://foo.example/foo")
    (t "ftp://foo.example/" "//bar.com/ok" "ftp://bar.com/ok")
    (t "https://foo.example/a/b/c" "d" "https://foo.example/a/b/d")
    (t "http://foo.example/a/b/c" "/x/x" "http://foo.example/x/x")
    (t "http://foo.example/a/b/c" nil "http://foo.example/a/b/c")
    (t "http://foo.example/a/b/c#foo" "" "http://foo.example/a/b/c")
    (t "http://foo.example/a/b/" "../c" "http://foo.example/a/c")
    (t "http://foo.example/a/b/" ".././c" "http://foo.example/a/c")
    (t "http://foo.example/a/b/" "../c/../../d" "http://foo.example/d")))

(provide 'elfeed-lib-tests)

;;; elfeed-lib-tests.el ends here
