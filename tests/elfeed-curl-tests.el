;;; elfeed-curl-tests.el --- curl tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'elfeed-lib)

(ert-deftest elfeed-curl--protocol-type ()
  (let ((table '((gopher . "gopher://sdf.org/1")
                 (http   . "http://feeds.bbci.co.uk/news/world/rss.xml")
                 (http   . "feeds.reuters.com/reuters/technologyNews")
                 (http   . "https://krebsonsecurity.com/feed/")
                 (file   . "file:///var/www/feed.xml"))))
    (cl-loop for (type . url) in table
             do (should (eq (elfeed-curl--protocol-type url) type)))))

(provide 'elfeed-curl-tests)

;;; elfeed-curl-tests.el ends here
