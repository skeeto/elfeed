;;; xml-query.el --- query engine complimenting the xml package

;;; Code:

(require 'cl)

(defun xml-query--tag-all (match xml)
  (loop for (tag attribs . content) in (remove-if-not #'listp xml)
        when (eq tag match)
        collect (cons tag (cons attribs content))))

(defun xml-query--attrib-all (attrib value xml)
  (loop for (tag attribs . content) in (remove-if-not #'listp xml)
        when (equal (cdr (assoc attrib attribs)) value)
        collect (cons tag (cons attribs content))))

(defun xml-query--keyword (matcher xml)
  (loop with match = (intern (substring (symbol-name matcher) 1))
        for (tag attribs . content) in (remove-if-not #'listp xml)
        when (cdr (assoc match attribs))
        collect it))

(defun xml-query--symbol (matcher xml)
  (xml-query--tag-all matcher xml))

(defun xml-query--vector (matcher xml)
  (let ((attrib (aref matcher 0))
        (value (aref matcher 1)))
    (xml-query--attrib-all attrib value xml)))

(defun xml-query--append (xml)
  (loop for (tag attribs . content) in (remove-if-not #'listp xml)
        append content))

(defun xml-query-all (query xml)
  "Given a list of tags, XML, apply QUERY and return a list of
matching tags.

A query is a list of matchers.
 - SYMBOL: filters to matching tags
 - VECTOR: filters to tags with matching attribute, [tag attrib value]
 - KEYWORD: filters to an attribute value (must be last)
 - * (an asterisk symbol): filters to content strings (must be last)

For example, to find all the 'alternate' link URL in a typical
Atom feed:

  (xml-query-all '(feed entry link [rel \"alternate\"] :href) xml)"
  (if (null query)
      xml
    (destructuring-bind (matcher . rest) query
      (cond
       ((keywordp matcher) (xml-query--keyword matcher xml))
       ((eq matcher '*) (remove-if-not #'stringp (xml-query--append xml)))
       (:else
        (let ((matches
               (typecase matcher
                 (symbol (xml-query--symbol matcher xml))
                 (vector (xml-query--vector matcher xml)))))
          (cond
           ((null rest) matches)
           ((keywordp (car rest)) (xml-query-all rest matches))
           ((eq '* (car rest)) (xml-query-all rest matches))
           (:else
            (xml-query-all (cdr query) (xml-query--append matches))))))))))

(defun xml-query (query xml)
  "Like `xml-query-all' but only return the first result."
  (car (xml-query-all query xml)))

(xml-query-all '(:xmlns) foo)

(xml-query-all '(feed entry) foo)

(xml-query-all '(feed entry title *) foo)

(xml-query-all '(feed entry [rel "alternate"] :href) foo)

(provide 'xml-query)

;;; xml-query.el ends here
