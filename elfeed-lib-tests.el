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

(provide 'elfeed-lib-tests)

;;; elfeed-lib-tests.el ends here
