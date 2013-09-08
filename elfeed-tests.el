;;; elfeed-tests.el --- tests for elfeed -*- lexical-binding: t; -*-

;; emacs -batch -Q -L . -l elfeed-tests.el -f ert-run-tests-batch

(require 'ert)
(require 'elfeed)
(require 'xml-query-tests)
(require 'elfeed-db-tests)
(require 'elfeed-lib-tests)

(defvar elfeed-test-rss
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss version=\"2.0\">
<channel>
 <title>RSS Title</title>
 <description>This is an example of an RSS feed</description>
 <link>http://www.example.com/main.html</link>
 <lastBuildDate>Mon, 06 Sep 2014 00:01:00 +0000 </lastBuildDate>
 <pubDate>Mon, 05 Sep 2014 16:20:00 +0000 </pubDate>
 <ttl>1800</ttl>

 <item>
  <title>Example entry 1</title>
  <description>Interesting description 1.</description>
  <link>http://www.nullprogram.com/</link>
  <guid>84815091-a6a3-35d4-7f04-80a6610dc85c</guid>
  <pubDate>Mon, 06 Sep 2009 16:20:00 +0000 </pubDate>
 </item>

 <item>
  <title>Example entry 2</title>
  <description>Interesting description 2.</description>
  <link>http://www.wikipedia.org/</link>
  <guid>5059196a-7f8e-3678-ecfe-dad84511d76f</guid>
  <pubDate>Mon,  2 Sep 2013 20:25:07 GMT</pubDate>
 </item>

</channel>
</rss>")

(defvar elfeed-test-atom
"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>Example Feed</title>
  <subtitle>A subtitle.</subtitle>
  <link href=\"http://example.org/feed/\" rel=\"self\"/>
  <link href=\"http://example.org/\"/>
  <id>urn:uuid:60a76c80-d399-11d9-b91C-0003939e0af6</id>
  <updated>2003-12-13T18:30:02Z</updated>

  <entry>
    <title>Atom-Powered Robots Run Amok</title>
    <link href=\"http://example.org/2003/12/13/atom03\"/>
    <link rel=\"alternate\" type=\"text/html\"
          href=\"http://example.org/2003/12/13/atom03.html\"/>
    <link rel=\"edit\" href=\"http://example.org/2003/12/13/atom03/edit\"/>
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
    <updated>2003-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
    <author>
      <name>John Doe</name>
      <email>johndoe@example.com</email>
    </author>
  </entry>

  <entry>
    <title>It's Raining Cats and Dogs</title>
    <link href=\"http://example.org/2004/12/13/atom03\"/>
    <link rel=\"alternate\" type=\"text/html\"
          href=\"http://example.org/2004/12/13/atom03.html\"/>
    <link rel=\"edit\" href=\"http://example.org/2004/12/13/atom03/edit\"/>
    <id>urn:uuid:1b91e3d7-2dac-3916-27a3-8d31566f2d09</id>
    <updated>2004-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
    <author>
      <name>John Doe</name>
      <email>johndoe@example.com</email>
    </author>
  </entry>
</feed>")

(defvar elfeed-test-rss1.0
  "<?xml version=\"1.0\"?>
<rdf:RDF
  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
  xmlns=\"http://purl.org/rss/1.0/\">
  <channel rdf:about=\"http://www.xml.com/xml/news.rss\">
    <title>XML.com</title>
    <link>http://xml.com/pub</link>
    <description>
      XML.com features a rich mix of information and services
      for the XML community.
    </description>
    <image rdf:resource=\"http://xml.com/universal/images/xml_tiny.gif\" />
    <items>
      <rdf:Seq>
        <rdf:li resource=\"http://xml.com/pub/2000/08/09/xslt/xslt.html\" />
        <rdf:li resource=\"http://xml.com/pub/2000/08/09/rdfdb/index.html\" />
      </rdf:Seq>
    </items>
  </channel>
  <item rdf:about=\"http://xml.com/pub/2000/08/09/xslt/xslt.html\">
    <title>Processing Inclusions with XSLT</title>
    <link>http://xml.com/pub/2000/08/09/xslt/xslt.html</link>
    <description>
     Processing document inclusions with general XML tools can be
     problematic. This article proposes a way of preserving inclusion
     information through SAX-based processing.
    </description>
  </item>
  <item rdf:about=\"http://xml.com/pub/2000/08/09/rdfdb/index.html\">
    <title>Putting RDF to Work</title>
    <link>http://xml.com/pub/2000/08/09/rdfdb/index.html</link>
    <description>
     Tool and API support for the Resource Description Framework
     is slowly coming of age. Edd Dumbill takes a look at RDFDB,
     one of the most exciting new RDF toolkits.
    </description>
  </item>
</rdf:RDF>")

(defvar elfeed-test-opml
  "<?xml version=\"1.0\"?>
<opml version=\"1.0\">
  <head>
    <title>Web Feeds</title>
  </head>
  <body>
    <outline title=\"Subscriptions\">
      <outline xmlUrl=\"http://example.com/feed/\" title=\"example\"/>
      <outline xmlUrl=\"http://foo.example.com/atom.xml\" title=\"foo\"/>
    </outline>
    <outline title=\"Comics\">
      <outline title=\"Funny\">
        <outline xmlUrl=\"http://funny.example.com/feed/\" title=\"funny\"/>
        <outline xmlUrl=\"http://boring.example.com/rss/\" title=\"boring\"/>
      </outline>
    </outline>
  </body>
</opml>")

(ert-deftest elfeed-float-time ()
  (macrolet ((test (time seconds)
                   `(should (= (elfeed-float-time ,time) ,seconds))))
    (test "1985-03-24T03:23:42Z"          480482622.0)
    (test "Mon,  5 May 1986 15:16:09 GMT" 515690169.0)))

(ert-deftest elfeed-feed-type ()
  (with-temp-buffer
    (insert elfeed-test-rss)
    (goto-char (point-min))
    (should (eq (elfeed-feed-type (xml-parse-region)) :rss)))
  (with-temp-buffer
    (insert elfeed-test-atom)
    (goto-char (point-min))
    (should (eq (elfeed-feed-type (xml-parse-region)) :atom)))
  (with-temp-buffer
    (insert elfeed-test-rss1.0)
    (goto-char (point-min))
    (should (eq (elfeed-feed-type (xml-parse-region)) :rss1.0))))

(ert-deftest elfeed-cleanup ()
  (should (string= (elfeed-cleanup "  foo  bar\n") "foo  bar"))
  (should (string= (elfeed-cleanup "foo\nbar") "foo bar")))

(ert-deftest elfeed-entries-from-x ()
  (with-elfeed-test
    (with-temp-buffer
      (insert elfeed-test-rss)
      (goto-char (point-min))
      (let ((url (elfeed-test-generate-url))
            (xml (xml-parse-region)))
        (destructuring-bind (a b) (elfeed-entries-from-rss url xml)
          (should (string= (elfeed-feed-title (elfeed-db-get-feed url))
                           "RSS Title"))
          (should (string= (elfeed-entry-title a) "Example entry 1"))
          (should (= (elfeed-entry-date a) 1252254000.0))
          (should (equal (elfeed-entry-id a)
                         (cons url "84815091-a6a3-35d4-7f04-80a6610dc85c")))
          (should (string= (elfeed-entry-title b) "Example entry 2"))
          (should (= (elfeed-entry-date b) 1378153507.0))
          (should (equal (elfeed-entry-id b)
                         (cons url "5059196a-7f8e-3678-ecfe-dad84511d76f"))))))
    (with-temp-buffer
      (insert elfeed-test-atom)
      (goto-char (point-min))
      (let ((url (elfeed-test-generate-url))
            (xml (xml-parse-region)))
        (destructuring-bind (a b) (elfeed-entries-from-atom url xml)
          (should (string= (elfeed-feed-title (elfeed-db-get-feed url))
                           "Example Feed"))
          (should (string= (elfeed-entry-title a)
                           "Atom-Powered Robots Run Amok"))
          (should (= (elfeed-entry-date a) 1071340202.0))
          (should
           (equal (elfeed-entry-id a)
                  (cons url "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a")))
          (should (string= (elfeed-entry-title b)
                           "It's Raining Cats and Dogs"))
          (should (= (elfeed-entry-date b) 1102962602.0))
          (should
           (equal (elfeed-entry-id b)
                  (cons url
                        "urn:uuid:1b91e3d7-2dac-3916-27a3-8d31566f2d09"))))))
    (with-temp-buffer
      (insert elfeed-test-rss1.0)
      (goto-char (point-min))
      (let ((url (elfeed-test-generate-url))
            (xml (xml-parse-region)))
        (destructuring-bind (a b) (elfeed-entries-from-rss1.0 url xml)
          (should (string= (elfeed-feed-title (elfeed-db-get-feed url))
                           "XML.com"))
          (should (string= (elfeed-entry-title a)
                           "Processing Inclusions with XSLT"))
          (should
           (equal (elfeed-entry-id a)
                  (cons url "http://xml.com/pub/2000/08/09/xslt/xslt.html")))
          (should (string= (elfeed-entry-title b)
                           "Putting RDF to Work"))
          (should
           (equal (elfeed-entry-id b)
                  (cons url
                        "http://xml.com/pub/2000/08/09/rdfdb/index.html"))))))))

(ert-deftest elfeed-tagger ()
  (with-elfeed-test
    (let* ((feed (elfeed-test-generate-feed))
           (tagger (elfeed-make-tagger :after "1 year ago"
                                       :entry-title "foobar"
                                       :feed-title '(not "exclude"))))
      (setf (elfeed-feed-title feed) "exclude this")
      (should-not
       (funcall tagger (make-elfeed-entry
                        :title "welcome to foobar: enjoy your stay"
                        :date (elfeed-float-time "6 months ago")
                        :feed-id (elfeed-feed-id feed))))
      (setf (elfeed-feed-title feed) "now include this")
      (should
       (funcall tagger (make-elfeed-entry
                        :title "welcome to foobar: enjoy your stay"
                        :date (elfeed-float-time "6 months ago")
                        :feed-id (elfeed-feed-id feed))))
      ;; May fail if this test takes more than 2 months to run!
      (should-not
       (funcall tagger (make-elfeed-entry
                        :title "welcome to foobar: enjoy your stay"
                        :date (elfeed-float-time "14 months ago")
                        :feed-id (elfeed-feed-id feed))))
      (should-not
       (funcall tagger (make-elfeed-entry
                        :title "welcome to barfoo: enjoy your stay"
                        :date (elfeed-float-time "1 month ago")
                        :feed-id (elfeed-feed-id feed)))))))

(ert-deftest elfeed-opml ()
  (let ((elfeed-feeds nil)
        (file (make-temp-file "feedlist")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert elfeed-test-opml))
          (elfeed-load-opml file)
          (setq elfeed-feeds (sort elfeed-feeds #'string<))
          (should (equal elfeed-feeds
                         '("http://boring.example.com/rss/"
                           "http://example.com/feed/"
                           "http://foo.example.com/atom.xml"
                           "http://funny.example.com/feed/"))))
      (ignore-errors (delete-file file))))
  (with-elfeed-test
    (let* ((outfile (make-temp-file "opml"))
           (feeds (loop repeat 10 collect (elfeed-test-generate-url)))
           (elfeed-feeds feeds))
      (unwind-protect
          (progn
            (loop for url in elfeed-feeds
                  for feed = (elfeed-db-get-feed url)
                  for title = (elfeed-test-generate-title)
                  do (setf (elfeed-feed-title feed) title))
            (elfeed-export-opml outfile)
            (setf elfeed-feeds nil)
            (elfeed-load-opml outfile)
            (setf elfeed-feeds (sort elfeed-feeds #'string<))
            (setf feeds (sort feeds #'string<))
            (should (equal elfeed-feeds feeds)))
        (ignore-errors (delete-file outfile))))))

(provide 'elfeed-tests)

;;; elfeed-tests.el ends here
