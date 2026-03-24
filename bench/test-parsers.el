;;; test-parsers.el --- Compare xml-parse-region vs libxml-parse-xml-region -*- lexical-binding: t; -*-

;; For each downloaded feed:
;; 1. Parse with both parsers
;; 2. Run elfeed's xml-query patterns against both results
;; 3. Measure timing
;; 4. Report correctness and performance

(require 'xml)
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))
(require 'elfeed-lib)
(require 'xml-query)

(defvar test-iterations 5
  "Number of iterations for timing each parser.")

(defun test--url-for-hash (feed-dir hash)
  "Read the URL from the .meta file for HASH in FEED-DIR."
  (let ((meta (expand-file-name (concat hash ".meta") feed-dir)))
    (when (file-exists-p meta)
      (with-temp-buffer
        (insert-file-contents meta)
        (string-trim (buffer-string))))))

(defun test--parse-old (content)
  "Parse CONTENT with xml-parse-region (symbol-qnames)."
  (with-temp-buffer
    (insert content)
    (let ((xml-default-ns ()))
      (xml-parse-region (point-min) (point-max) nil nil 'symbol-qnames))))

(defun test--parse-new (content)
  "Parse CONTENT with libxml-parse-xml-region, using the same unwrap as production."
  (with-temp-buffer
    (insert content)
    (elfeed-xml--libxml-unwrap (libxml-parse-xml-region (point-min) (point-max)))))

(defun test--feed-type (xml)
  "Determine feed type from parsed XML."
  (let ((top (xml-query-strip-ns (caar xml))))
    (cadr (assoc top '((feed :atom) (rss :rss) (RDF :rss1.0))))))

(defun test--queries-for-type (type)
  "Return list of (name . query-fn) for feed TYPE."
  (pcase type
    (:atom
     `(("feed-type"          . ,(lambda (xml) (xml-query-strip-ns (caar xml))))
       ("feed-title"         . ,(lambda (xml) (xml-query* (feed title *) xml)))
       ("entry-titles"       . ,(lambda (xml) (xml-query-all* (feed entry title *) xml)))
       ("entry-alt-link"     . ,(lambda (xml) (xml-query* (feed entry link [rel "alternate"] :href) xml)))
       ("entry-any-link"     . ,(lambda (xml) (xml-query* (feed entry link :href) xml)))
       ("entry-id"           . ,(lambda (xml) (xml-query* (feed entry id *) xml)))
       ("entry-published"    . ,(lambda (xml) (xml-query* (feed entry published *) xml)))
       ("entry-updated"      . ,(lambda (xml) (xml-query* (feed entry updated *) xml)))
       ("entry-date"         . ,(lambda (xml) (xml-query* (feed entry date *) xml)))
       ("entry-content-type" . ,(lambda (xml) (xml-query* (feed entry content :type) xml)))
       ("entry-content"      . ,(lambda (xml) (xml-query* (feed entry content *) xml)))
       ("entry-summary"      . ,(lambda (xml) (xml-query* (feed entry summary *) xml)))
       ("entry-authors"      . ,(lambda (xml) (xml-query-all* (feed entry author) xml)))
       ("entry-creators"     . ,(lambda (xml) (xml-query-all* (feed entry creator *) xml)))
       ("entry-categories"   . ,(lambda (xml) (xml-query-all* (feed entry category :term) xml)))
       ("entry-base"         . ,(lambda (xml) (xml-query* (feed entry :base) xml)))
       ("enclosure-links"    . ,(lambda (xml) (xml-query-all* (feed entry link [rel "enclosure"]) xml)))
       ("feed-author-name"   . ,(lambda (xml) (xml-query* (feed author name *) xml)))
       ("feed-authors"       . ,(lambda (xml) (xml-query-all* (feed author) xml)))))
    (:rss
     `(("feed-type"          . ,(lambda (xml) (xml-query-strip-ns (caar xml))))
       ("channel-title"      . ,(lambda (xml) (xml-query* (rss channel title *) xml)))
       ("item-titles"        . ,(lambda (xml) (xml-query-all* (rss channel item title *) xml)))
       ("item-link"          . ,(lambda (xml) (xml-query* (rss channel item link *) xml)))
       ("item-guid"          . ,(lambda (xml) (xml-query* (rss channel item guid *) xml)))
       ("item-pubDate"       . ,(lambda (xml) (xml-query* (rss channel item pubDate *) xml)))
       ("item-date"          . ,(lambda (xml) (xml-query* (rss channel item date *) xml)))
       ("item-author"        . ,(lambda (xml) (xml-query* (rss channel item author *) xml)))
       ("item-creator"       . ,(lambda (xml) (xml-query* (rss channel item creator *) xml)))
       ("item-categories"    . ,(lambda (xml) (xml-query-all* (rss channel item category *) xml)))
       ("item-description"   . ,(lambda (xml) (xml-query-all* (rss channel item description *) xml)))
       ("item-encoded"       . ,(lambda (xml) (xml-query-all* (rss channel item encoded *) xml)))
       ("enclosure-url"      . ,(lambda (xml) (xml-query* (rss channel item enclosure :url) xml)))))
    (:rss1.0
     `(("feed-type"          . ,(lambda (xml) (xml-query-strip-ns (caar xml))))
       ("channel-title"      . ,(lambda (xml) (xml-query* (RDF channel title *) xml)))
       ("item-titles"        . ,(lambda (xml) (xml-query-all* (RDF item title *) xml)))
       ("item-link"          . ,(lambda (xml) (xml-query* (RDF item link *) xml)))
       ("item-date"          . ,(lambda (xml) (xml-query* (RDF item date *) xml)))
       ("item-description"   . ,(lambda (xml) (xml-query-all* (RDF item description *) xml)))))))

(defun test--time-parse (parse-fn content iterations)
  "Time PARSE-FN over CONTENT for ITERATIONS, return total seconds."
  (let ((start (float-time)))
    (dotimes (_ iterations)
      (funcall parse-fn content))
    (- (float-time) start)))

(defun test--run-feed (file feed-dir)
  "Test a single feed FILE. Return a result plist."
  (let* ((hash (file-name-sans-extension (file-name-nondirectory file)))
         (url (or (test--url-for-hash feed-dir hash) "unknown"))
         (content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
         (size (length content)))
    (condition-case err
        (let* ((old (test--parse-old content))
               (new (test--parse-new content))
               (type-old (test--feed-type old))
               (type-new (test--feed-type new))
               (queries (test--queries-for-type type-old))
               (failures ()))

          ;; Check type match
          (unless (eq type-old type-new)
            (push (format "feed-type mismatch: %S vs %S" type-old type-new) failures))

          ;; Skip if unknown type
          (unless type-old
            (signal 'error (list (format "Unknown feed type (top: %S)" (caar old)))))

          ;; Run all queries
          (dolist (q queries)
            (let* ((name (car q))
                   (fn (cdr q))
                   (r-old (condition-case e (funcall fn old) (error (format "ERROR:%S" e))))
                   (r-new (condition-case e (funcall fn new) (error (format "ERROR:%S" e)))))
              (unless (equal r-old r-new)
                (push (format "%s: old=%S new=%S" name r-old r-new) failures))))

          ;; Timing
          (let ((time-old (test--time-parse #'test--parse-old content test-iterations))
                (time-new (test--time-parse #'test--parse-new content test-iterations)))
            (list :url url :hash hash :size size
                  :type type-old
                  :queries-total (length queries)
                  :queries-failed (length failures)
                  :failures failures
                  :time-old time-old :time-new time-new
                  :speedup (if (> time-new 0) (/ time-old time-new) 0.0)
                  :status (if failures "FAIL" "PASS"))))
      (error
       (list :url url :hash hash :size size
             :type nil
             :queries-total 0 :queries-failed 0 :failures nil
             :time-old 0.0 :time-new 0.0 :speedup 0.0
             :status (format "PARSE-ERROR: %S" err))))))

(defun test--run-all (feed-dir output-file)
  "Run tests on all .xml files in FEED-DIR, write report to OUTPUT-FILE."
  (let* ((files (directory-files feed-dir t "\\.xml$"))
         (total (length files))
         (results ())
         (pass 0) (fail 0) (err 0)
         (total-time-old 0.0) (total-time-new 0.0)
         (n 0))

    (dolist (file files)
      (cl-incf n)
      (let ((result (test--run-feed file feed-dir)))
        (push result results)
        (let ((status (plist-get result :status)))
          (cond ((equal status "PASS") (cl-incf pass))
                ((equal status "FAIL") (cl-incf fail))
                (t (cl-incf err)))
          (cl-incf total-time-old (plist-get result :time-old))
          (cl-incf total-time-new (plist-get result :time-new))
          ;; Progress
          (princ (format "[%3d/%d] %-4s %6d bytes  %s\n"
                         n total status (plist-get result :size)
                         (plist-get result :url))))))

    (setf results (nreverse results))

    ;; Write detailed report
    (with-temp-file output-file
      (insert (format "Elfeed XML Parser Comparison Report\n"))
      (insert (format "Generated: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S %Z")))
      (insert (format "Iterations per feed: %d\n" test-iterations))
      (insert (format "Emacs version: %s\n" emacs-version))
      (insert (format "libxml2 available: %s\n\n" (if (fboundp 'libxml-parse-xml-region) "yes" "no")))

      (insert (format "========================================\n"))
      (insert (format "SUMMARY\n"))
      (insert (format "========================================\n"))
      (insert (format "Feeds tested:  %d\n" total))
      (insert (format "  PASS:        %d\n" pass))
      (insert (format "  FAIL:        %d\n" fail))
      (insert (format "  PARSE-ERROR: %d\n\n" err))

      (insert (format "Total parse time (xml-parse-region):    %.3fs\n" total-time-old))
      (insert (format "Total parse time (libxml-parse-xml):    %.3fs\n" total-time-new))
      (when (> total-time-new 0)
        (insert (format "Overall speedup:                        %.1fx\n" (/ total-time-old total-time-new))))

      ;; Per-feed timing table
      (insert (format "\n========================================\n"))
      (insert (format "PER-FEED RESULTS (sorted by speedup)\n"))
      (insert (format "========================================\n"))
      (insert (format "%-6s %7s %10s %10s %7s  %s\n"
                       "Status" "Size" "Old(ms)" "New(ms)" "Speedup" "URL"))
      (insert (format "%s\n" (make-string 100 ?-)))

      (let ((sorted (sort (cl-remove-if-not
                           (lambda (r) (stringp (plist-get r :status)))
                           (cl-remove-if-not
                            (lambda (r) (member (plist-get r :status) '("PASS" "FAIL")))
                            results))
                          (lambda (a b) (> (plist-get a :speedup) (plist-get b :speedup))))))
        (dolist (r sorted)
          (insert (format "%-6s %7d %8.1fms %8.1fms %6.1fx  %s\n"
                          (plist-get r :status)
                          (plist-get r :size)
                          (* 1000 (/ (plist-get r :time-old) test-iterations))
                          (* 1000 (/ (plist-get r :time-new) test-iterations))
                          (plist-get r :speedup)
                          (plist-get r :url)))))

      ;; Failures detail
      (when (> fail 0)
        (insert (format "\n========================================\n"))
        (insert (format "FAILURE DETAILS\n"))
        (insert (format "========================================\n"))
        (dolist (r results)
          (when (equal (plist-get r :status) "FAIL")
            (insert (format "\n--- %s ---\n" (plist-get r :url)))
            (insert (format "  Type: %s, Size: %d bytes\n" (plist-get r :type) (plist-get r :size)))
            (insert (format "  Queries: %d total, %d failed\n"
                            (plist-get r :queries-total) (plist-get r :queries-failed)))
            (dolist (f (plist-get r :failures))
              (insert (format "    - %s\n" f))))))

      ;; Parse errors detail
      (when (> err 0)
        (insert (format "\n========================================\n"))
        (insert (format "PARSE ERRORS\n"))
        (insert (format "========================================\n"))
        (dolist (r results)
          (unless (member (plist-get r :status) '("PASS" "FAIL"))
            (insert (format "\n  %s\n    %s\n" (plist-get r :url) (plist-get r :status)))))))

    ;; Summary to stdout
    (princ (format "\n========================================\n"))
    (princ (format "PASS: %d  FAIL: %d  ERROR: %d  (of %d feeds)\n" pass fail err total))
    (princ (format "xml-parse-region total:  %.3fs\n" total-time-old))
    (princ (format "libxml-parse-xml total:  %.3fs\n" total-time-new))
    (when (> total-time-new 0)
      (princ (format "Overall speedup:        %.1fx\n" (/ total-time-old total-time-new))))
    (princ (format "Report: %s\n" output-file))))

;; Entry point
(let* ((feed-dir (car command-line-args-left))
       (output-file (cadr command-line-args-left)))
  (test--run-all feed-dir output-file))
