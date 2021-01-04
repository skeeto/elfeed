;;; elfeed-tests.el --- tests for elfeed -*- lexical-binding: t; -*-

;; emacs -batch -Q -L .. -L . -l elfeed-tests.el -f ert-run-tests-batch

(require 'ert)
(require 'elfeed)
(require 'elfeed-lib)
(require 'xml-query-tests)
(require 'elfeed-db-tests)
(require 'elfeed-lib-tests)
(require 'elfeed-search-tests)
(require 'elfeed-curl-tests)

(defvar elfeed-test-rss
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss version=\"2.0\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\">
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
  <link>
    <![CDATA[http://nullprogram.com/]]>
  </link>
  <author>john.doe@example.com (John Doe)</author>
  <guid>84815091-a6a3-35d4-7f04-80a6610dc85c</guid>
  <pubDate>Mon, 06 Sep 2009 16:20:00 +0000 </pubDate>
  <category>example-entry</category>
  <category>Example One</category>
 </item>

 <item>
  <title>Example entry 2</title>
  <description>Interesting description 2.</description>
  <link>http://www.wikipedia.org/</link>
  <dc:creator>Jane Doe &lt;jane.doe@example.com&gt;</dc:creator>
  <dc:creator>Baby Doe &lt;baby.doe@example.com&gt;</dc:creator>
  <guid>5059196a-7f8e-3678-ecfe-dad84511d76f</guid>
  <pubDate>Mon,  2 Sep 2013 20:25:07 GMT</pubDate>
  <category>example-entry</category>
  <category>Example Two</category>
 </item>

</channel>
</rss>")

(defvar elfeed-test-atom
"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\" xml:base=\"http://example.org/\">
  <title>Example Feed</title>
  <subtitle>A subtitle.</subtitle>
  <link href=\"http://example.org/feed/\" rel=\"self\"/>
  <link href=\"http://example.org/\"/>
  <id>urn:uuid:60a76c80-d399-11d9-b91C-0003939e0af6</id>
  <updated>2003-12-13T18:30:02Z</updated>
  <author>
    <name>John Doe (feed)</name>
    <email>johndoe@example.com</email>
  </author>
  <author>
    <name>Jane Doe (feed)</name>
    <email>janedoe@example.com</email>
  </author>

  <entry xml:base=\"/2003/12/13/\">
    <title>Atom-Powered Robots Run Amok</title>
    <link href=\"atom03\"/>
    <link rel=\"alternate\" type=\"text/html\"
          href=\"/2003/atom03.html\"/>
    <link rel=\"edit\" href=\"atom03/edit\"/>
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
    <updated>2003-12-13T18:30:02Z</updated>
    <category scheme=\"http://example.com/scheme/\" term=\"example\"/>
    <category term=\"cat-1\" scheme=\"http://example.com/scheme/\"/>
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
    <category term=\"example\"/>
    <category term=\"cat-2\"/>
    <summary>Some text.</summary>
    <author>
      <name>John Doe</name>
      <email>johndoe@example.com</email>
    </author>
    <author>
      <name>Jane Doe</name>
      <email>janedoe@example.com</email>
    </author>
    <dc:creator>Foo Bar</dc:creator>
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

(defvar elfeed-test-xml-base
  "<?xml version='1.0' encoding='utf-8'?>
<feed xml:base='http://foo.example.org/'>
  <title>xml:base test</title>
  <subtitle>xml:base is complicated</subtitle>
  <link href='/feed/' rel='self'/>
  <link href='/'/>
  <id>urn:uuid:1edeb49c-1f0a-3de3-9a37-9802ef5c0add</id>
  <updated>2015-12-13T18:30:02Z</updated>
  <author>
    <name>xml:base</name>
    <email>xml@base.example.com</email>
  </author>

  <entry xml:base='/entry0/'>
    <title>Entry 0</title>
    <link rel='alternate' type='text/html' href='content0.html'/>
    <id>urn:uuid:b42c623a-fbf0-31c8-3d54-1a56ee88e6a4</id>
    <updated>2015-12-13T18:30:02Z</updated>
    <content>Content 0</content>
  </entry>

  <entry>
    <title>Entry 1</title>
    <link rel='alternate' type='text/html' href='/entry1/content1.html'/>
    <id>urn:uuid:bdc21cd1-ceac-3439-ea05-3a1d34796dd2</id>
    <updated>2016-12-13T18:30:02Z</updated>
    <summary>Content 1</summary>
  </entry>

  <entry xml:base='https://entry2.example.com/entry2/'>
    <title>Entry 1</title>
    <link rel='alternate' type='text/html' href='content2.html'/>
    <id>urn:uuid:bdc21cd1-ceac-3439-ea05-3a1d34796dd2</id>
    <updated>2016-12-13T18:30:02Z</updated>
    <summary>Content 1</summary>
  </entry>
</feed>")

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

(ert-deftest elfeed-feed-type ()
  (with-temp-buffer
    (insert elfeed-test-rss)
    (should (eq (elfeed-feed-type (elfeed-xml-parse-region)) :rss)))
  (with-temp-buffer
    (insert elfeed-test-atom)
    (should (eq (elfeed-feed-type (elfeed-xml-parse-region)) :atom)))
  (with-temp-buffer
    (insert elfeed-test-rss1.0)
    (should (eq (elfeed-feed-type (elfeed-xml-parse-region)) :rss1.0))))

(ert-deftest elfeed-entries-from-x ()
  (with-elfeed-test
    (with-temp-buffer
      (insert elfeed-test-rss)
      (goto-char (point-min))
      (let* ((url (elfeed-test-generate-url))
             (namespace (elfeed-url-to-namespace url))
             (xml (elfeed-xml-parse-region)))
        (cl-destructuring-bind (a b) (elfeed-entries-from-rss url xml)
          (should (string= (elfeed-feed-title (elfeed-db-get-feed url))
                           "RSS Title"))
          (should (string= (elfeed-entry-title a) "Example entry 1"))
          (should (string= (elfeed-entry-link a) "http://nullprogram.com/"))
          (should (= (elfeed-entry-date a) 1252254000.0))
          (should (equal (elfeed-entry-id a)
                         (cons namespace
                               "84815091-a6a3-35d4-7f04-80a6610dc85c")))
          (should (string= (plist-get (nth 0 (elfeed-meta a :authors)) :name)
                           "John Doe"))
          (should (string= (plist-get (nth 0 (elfeed-meta a :authors)) :email)
                           "john.doe@example.com"))
          (should (string= (elfeed-entry-title b) "Example entry 2"))
          (should (= (elfeed-entry-date b) 1378153507.0))
          (should (equal (elfeed-entry-id b)
                         (cons namespace
                               "5059196a-7f8e-3678-ecfe-dad84511d76f")))
          (should (string= (plist-get (nth 0 (elfeed-meta b :authors)) :name)
                           "Jane Doe <jane.doe@example.com>"))
          (should (string= (plist-get (nth 1 (elfeed-meta b :authors)) :name)
                           "Baby Doe <baby.doe@example.com>"))
          (should (member "example-entry" (elfeed-meta b :categories)))
          (should (member "Example Two" (elfeed-meta b :categories))))))
    (with-temp-buffer
      (insert elfeed-test-atom)
      (goto-char (point-min))
      (let* ((url (elfeed-test-generate-url))
             (namespace (elfeed-url-to-namespace url))
             (xml (elfeed-xml-parse-region))
             (feed (elfeed-db-get-feed url)))
        (cl-destructuring-bind (a b) (elfeed-entries-from-atom url xml)
          ;; Authors
          (should (string= (plist-get (nth 0 (elfeed-feed-author feed)) :name)
                           "John Doe (feed)"))
          (should (string= (plist-get (nth 0 (elfeed-feed-author feed)) :email)
                           "johndoe@example.com"))
          (should (string= (plist-get (nth 1 (elfeed-feed-author feed)) :name)
                           "Jane Doe (feed)"))
          (should (string= (plist-get (nth 1 (elfeed-feed-author feed)) :email)
                           "janedoe@example.com"))
          ;; Titles
          (should (string= (elfeed-feed-title (elfeed-db-get-feed url))
                           "Example Feed"))
          (should (string= (elfeed-entry-title a)
                           "Atom-Powered Robots Run Amok"))
          ;; Entry A
          (should (string= (elfeed-entry-link a)
                           "http://example.org/2003/atom03.html"))
          (should (= (elfeed-entry-date a) 1071340202.0))
          (should
           (equal (elfeed-entry-id a)
                  (cons namespace
                        "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a")))
          (should (member "example" (elfeed-meta a :categories)))
          (should (member "cat-1" (elfeed-meta a :categories)))
          ;; Entry B
          (should (string= (elfeed-entry-title b)
                           "It's Raining Cats and Dogs"))
          (should (string= (elfeed-entry-link b)
                           "http://example.org/2004/12/13/atom03.html"))
          (should (= (elfeed-entry-date b) 1102962602.0))
          (should (string= (plist-get (nth 0 (elfeed-meta b :authors)) :name)
                           "John Doe"))
          (should (string= (plist-get (nth 0 (elfeed-meta b :authors)) :email)
                           "johndoe@example.com"))
          (should (string= (plist-get (nth 1 (elfeed-meta b :authors)) :name)
                           "Jane Doe"))
          (should (string= (plist-get (nth 1 (elfeed-meta b :authors)) :email)
                           "janedoe@example.com"))
          (should (member "example" (elfeed-meta b :categories)))
          (should (member "cat-2" (elfeed-meta b :categories)))
          (should
           (equal (elfeed-entry-id b)
                  (cons namespace
                        "urn:uuid:1b91e3d7-2dac-3916-27a3-8d31566f2d09"))))))
    (with-temp-buffer
      (insert elfeed-test-rss1.0)
      (goto-char (point-min))
      (let* ((url (elfeed-test-generate-url))
             (namespace (elfeed-url-to-namespace url))
             (xml (elfeed-xml-parse-region)))
        (cl-destructuring-bind (a b) (elfeed-entries-from-rss1.0 url xml)
          (should (string= (elfeed-feed-title (elfeed-db-get-feed url))
                           "XML.com"))
          (should (string= (elfeed-entry-title a)
                           "Processing Inclusions with XSLT"))
          (should
           (equal (elfeed-entry-id a)
                  (cons namespace
                        "http://xml.com/pub/2000/08/09/xslt/xslt.html")))
          (should (string= (elfeed-entry-title b)
                           "Putting RDF to Work"))
          (should
           (equal (elfeed-entry-id b)
                  (cons namespace
                        "http://xml.com/pub/2000/08/09/rdfdb/index.html"))))))))

(ert-deftest elfeed-protocol-relative-url ()
  (with-elfeed-test
    (with-temp-buffer
      (insert elfeed-test-rss)
      (setf (point) (point-min))
      (while (search-forward "http://" nil t)
        (replace-match "//" nil t))
      (setf (point) (point-min))
      (let ((xml (elfeed-xml-parse-region)))
        (cl-destructuring-bind (a b)
            (elfeed-entries-from-rss "http://example.com/" xml)
          (should (equal (elfeed-entry-link a)
                         "http://nullprogram.com/"))
          (should (equal (elfeed-entry-link b)
                         "http://www.wikipedia.org/")))
        (cl-destructuring-bind (a b)
            (elfeed-entries-from-rss "https://example.com/" xml)
          (should (equal (elfeed-entry-link a)
                         "https://nullprogram.com/"))
          (should (equal (elfeed-entry-link b)
                         "https://www.wikipedia.org/")))))
    (with-temp-buffer
      (insert elfeed-test-atom)
      (setf (point) (point-min))
      (while (search-forward "base=\"http://" nil t)
        (replace-match "base=\"//" nil t))
      (setf (point) (point-min))
      (let ((xml (elfeed-xml-parse-region)))
        (cl-destructuring-bind (a b)
            (elfeed-entries-from-atom "http://example.com/" xml)
          (should (equal (elfeed-entry-link a)
                         ;; inherited protocol-relative from xml:base
                         "http://example.org/2003/atom03.html"))
          (should (equal (elfeed-entry-link b)
                         "http://example.org/2004/12/13/atom03.html")))
        (cl-destructuring-bind (a b)
            (elfeed-entries-from-atom "https://example.com/" xml)
          (should (equal (elfeed-entry-link a)
                         ;; inherited protocol-relative from xml:base
                         "https://example.org/2003/atom03.html"))
          (should (equal (elfeed-entry-link b)
                         "http://example.org/2004/12/13/atom03.html")))))))

(ert-deftest elfeed-xml-base ()
  (with-elfeed-test
    (with-temp-buffer
      (insert elfeed-test-xml-base)
      (goto-char (point-min))
      (let* ((url "http://bar.example.org/")
             (xml (elfeed-xml-parse-region))
             (_feed (elfeed-db-get-feed url)))
        (cl-destructuring-bind (a b c) (elfeed-entries-from-atom url xml)
          (should (string=
                   (elfeed-entry-link a)
                   "http://foo.example.org/entry0/content0.html"))
          (should (string=
                   (elfeed-entry-link b)
                   "http://foo.example.org/entry1/content1.html"))
          (should (string=
                   (elfeed-entry-link c)
                   "https://entry2.example.com/entry2/content2.html")))))))

(ert-deftest elfeed-tagger ()
  (with-elfeed-test
    (let* ((feed (elfeed-test-generate-feed))
           (tagger (elfeed-make-tagger :after "1 year ago"
                                       :entry-title "foobar"
                                       :feed-title '(not "exclude"))))
      (setf (elfeed-feed-title feed) "exclude this")
      (should-not
       (funcall tagger (elfeed-entry--create
                        :title "welcome to foobar: enjoy your stay"
                        :date (elfeed-float-time "6 months ago")
                        :feed-id (elfeed-feed-id feed))))
      (setf (elfeed-feed-title feed) "now include this")
      (should
       (funcall tagger (elfeed-entry--create
                        :title "welcome to foobar: enjoy your stay"
                        :date (elfeed-float-time "6 months ago")
                        :feed-id (elfeed-feed-id feed))))
      ;; May fail if this test takes more than 2 months to run!
      (should-not
       (funcall tagger (elfeed-entry--create
                        :title "welcome to foobar: enjoy your stay"
                        :date (elfeed-float-time "14 months ago")
                        :feed-id (elfeed-feed-id feed))))
      (should-not
       (funcall tagger (elfeed-entry--create
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
           (feeds (cl-loop repeat 10 collect (elfeed-test-generate-url)))
           (elfeed-feeds feeds))
      (unwind-protect
          (progn
            (cl-loop for url in elfeed-feeds
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

(ert-deftest elfeed-autotags ()
  (let ((elfeed-feeds '("foo" ("bar" :tag-a tag-b) "baz" ("qux"))))
    (should (equal (elfeed-feed-list) '("foo" "bar" "baz" "qux")))
    (should (equal (elfeed-feed-autotags "foo") '()))
    (should (equal (elfeed-feed-autotags "qux") '()))
    (should (equal (elfeed-feed-autotags "bar") '(tag-a tag-b)))
    (should (equal (elfeed-feed-autotags (elfeed-feed--create :url "bar"))
                   '(tag-a tag-b))))
  (with-elfeed-test
    (with-temp-buffer
      (insert elfeed-test-atom)
      (goto-char (point-min))
      (let* ((elfeed-feeds '("http://bar/" ("http://foo/" tag-a :tag-b)))
             (xml (elfeed-xml-parse-region))
             (entry (cl-first (elfeed-entries-from-atom "http://foo/" xml))))
        (should (equal (elfeed-entry-tags entry)
                       (elfeed-normalize-tags '(unread tag-a tag-b))))))))

(provide 'elfeed-tests)

;;; elfeed-tests.el ends here
