.POSIX:
EMACS   = emacs
BATCH   = $(EMACS) -batch -Q -L . -L tests
VERSION = 3.4.1

EL   = elfeed-csv.el elfeed-curl.el elfeed-db.el elfeed-lib.el	\
       elfeed-log.el elfeed-show.el elfeed.el xml-query.el	\
       elfeed-search.el elfeed-link.el
DOC  = README.md NEWS.md UNLICENSE elfeed-pkg.el
WEB  = web/elfeed-web-pkg.el web/elfeed-web.el web/elfeed.css	\
       web/elfeed.js web/index.html
TEST = tests/elfeed-db-tests.el tests/elfeed-lib-tests.el       \
       tests/elfeed-tests.el tests/elfeed-search-tests.el       \
       tests/xml-query-tests.el

compile: $(EL:.el=.elc) $(TEST:.el=.elc)

check: test
test: $(EL:.el=.elc) $(TEST:.el=.elc)
	$(BATCH) -l tests/elfeed-tests.elc -f ert-run-tests-batch

package: elfeed-$(VERSION).tar elfeed-web-$(VERSION).tar

clean:
	rm -f *.tar $(EL:.el=.elc) $(TEST:.el=.elc)

virtual: compile
	(mkdir -p tmp-$$$$/.elfeed; \
	 cp ~/.elfeed/index tmp-$$$$/.elfeed/ 2>/dev/null || true; \
	 trap "rm -rf tmp-$$$$" INT EXIT; \
	 HOME=$$PWD/tmp-$$$$ $(EMACS) -L . -l elfeed.elc $(ARGS))

elfeed-$(VERSION).tar: $(EL) $(DOC)
	rm -rf elfeed-$(VERSION)/
	mkdir elfeed-$(VERSION)/
	cp $(EL) $(DOC) elfeed-$(VERSION)/
	tar cf $@ elfeed-$(VERSION)/
	rm -rf elfeed-$(VERSION)/

elfeed-web-$(VERSION).tar: $(WEB)
	rm -rf elfeed-web-$(VERSION)/
	mkdir elfeed-web-$(VERSION)/
	cp $(WEB) elfeed-web-$(VERSION)/
	tar cf $@ elfeed-web-$(VERSION)/
	rm -rf elfeed-web-$(VERSION)/

elfeed-csv.elc: elfeed-csv.el elfeed-db.elc
elfeed-curl.elc: elfeed-curl.el elfeed-lib.elc elfeed-log.elc
elfeed-db.elc: elfeed-db.el elfeed-lib.elc
elfeed-lib.elc: elfeed-lib.el
elfeed-log.elc: elfeed-log.el
elfeed-show.elc: elfeed-show.el elfeed.elc elfeed-db.elc elfeed-lib.elc \
    elfeed-search.elc
elfeed-link.elc: elfeed-link.el elfeed.elc elfeed-search.elc elfeed-show.elc
elfeed.elc: elfeed.el elfeed-lib.elc elfeed-log.elc elfeed-curl.elc \
    elfeed-db.elc xml-query.elc
xml-query.elc: xml-query.el
elfeed-search.elc: elfeed-search.el elfeed.elc elfeed-db.elc elfeed-lib.elc
tests/elfeed-db-tests.elc: tests/elfeed-db-tests.el elfeed-db.elc
tests/elfeed-lib-tests.elc: tests/elfeed-lib-tests.el elfeed-lib.elc
tests/elfeed-tests.elc: tests/elfeed-tests.el elfeed.elc
tests/xml-query-tests.elc: tests/xml-query-tests.el xml-query.elc

.SUFFIXES: .el .elc

.el.elc:
	$(BATCH) -f batch-byte-compile $<
