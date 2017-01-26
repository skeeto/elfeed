.POSIX :
EMACS    = emacs
BATCH    = $(EMACS) -batch -Q -L . -L tests
VERSION != $(BATCH) -l elfeed.el --eval '(princ elfeed-version)'

EL   = elfeed-csv.el elfeed-curl.el elfeed-db.el elfeed-lib.el	\
       elfeed-log.el elfeed-show.el elfeed.el xml-query.el	\
       elfeed-search.el
DOC  = README.md UNLICENSE elfeed-pkg.el
WEB  = web/elfeed-web-pkg.el web/elfeed-web.el web/elfeed.css	\
       web/elfeed.js web/index.html
TEST = tests/elfeed-db-tests.el tests/elfeed-lib-tests.el	\
       tests/elfeed-tests.el tests/xml-query-tests.el

compile : $(EL:.el=.elc) $(TEST:.el=.elc)

test : $(EL:.el=.elc) $(TEST:.el=.elc)
	$(BATCH) -l tests/elfeed-tests.el -f ert-run-tests-batch

package : elfeed-$(VERSION).tar elfeed-web-$(VERSION).tar

clean:
	rm -f *.tar $(EL:.el=.elc) $(TEST:.el=.elc)

elfeed-$(VERSION).tar : $(EL) $(DOC)
	tar -cf $@ --transform "s,^,elfeed-$(VERSION)/," $(EL) $(DOC)

elfeed-web-$(VERSION).tar : $(WEB)
	tar -cf $@ --transform "s,^web/,elfeed-web-$(VERSION)/," $(WEB)

elfeed-csv.elc : elfeed-csv.el
elfeed-curl.elc : elfeed-curl.el
elfeed-db.elc : elfeed-db.el
elfeed-lib.elc : elfeed-lib.el
elfeed-log.elc : elfeed-log.el
elfeed-show.elc : elfeed-show.el
elfeed.elc : elfeed.el
xml-query.elc : xml-query.el
elfeed-search.elc : elfeed-search.el
tests/elfeed-db-tests.elc : tests/elfeed-db-tests.el
tests/elfeed-lib-tests.elc : tests/elfeed-lib-tests.el
tests/elfeed-tests.elc : tests/elfeed-tests.el
tests/xml-query-tests.elc : tests/xml-query-tests.el

.PHONY : compile test package clean
.SUFFIXES : .el .elc

.el.elc :
	$(BATCH) -f batch-byte-compile $<
