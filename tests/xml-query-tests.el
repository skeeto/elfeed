;;; xml-query-tests.el -- tests for xml-query

(require 'ert)
(require 'xml-query)

(ert-deftest xml-query ()
  (let ((xml '((foo ((xmlns . "example/xml"))
                    (bar ((href . "example.com")) "FOO" (p ()) "BAR")
                    (baz () "FOOBAZ")))))
    (should (string= (xml-query '(foo :xmlns) xml) "example/xml"))
    (should (string= (xml-query* (foo :xmlns) xml) "example/xml"))
    (should (string= (xml-query '(foo bar :href) xml) "example.com"))
    (should (string= (xml-query* (foo bar :href) xml) "example.com"))
    (should (string= (xml-query '(foo baz *) xml) "FOOBAZ"))
    (should (string= (xml-query* (foo baz *) xml) "FOOBAZ"))
    (should (string= (xml-query '(foo bar *) xml) "FOO"))
    (should (string= (xml-query* (foo bar *) xml) "FOO"))
    (should (equal (xml-query-all '(foo bar *) xml) '("FOO" "BAR")))
    (should (equal (xml-query-all* (foo bar *) xml) '("FOO" "BAR")))
    (should (equal (xml-query-all '(foo baz *) xml) '("FOOBAZ")))
    (should (equal (xml-query-all* (foo baz *) xml) '("FOOBAZ")))
    (should (equal (xml-query-all '(foo (baz bar) *) xml)
                   '("FOOBAZ" "FOO" "BAR")))))

(provide 'xml-query-tests)

;;; xml-query-tests.el ends here
