.POSIX:
EMACS   = emacs
BATCH   = $(EMACS) -batch -Q -L . -L tests
VERSION = 3.4.2

EL   = elfeed-csv.el elfeed-curl.el elfeed-db.el elfeed-lib.el	\
       elfeed-log.el elfeed-show.el elfeed.el xml-query.el	\
       elfeed-search.el elfeed-link.el
DOC  = README.md NEWS.md UNLICENSE
TEST = tests/elfeed-db-tests.el tests/elfeed-lib-tests.el       \
       tests/elfeed-tests.el tests/elfeed-search-tests.el       \
       tests/elfeed-curl-tests.el tests/xml-query-tests.el

compile: $(EL:.el=.elc) $(TEST:.el=.elc)

check: test
test: $(EL:.el=.elc) $(TEST:.el=.elc)
	$(BATCH) -l tests/elfeed-tests.elc -f ert-run-tests-batch

package: elfeed-$(VERSION).tar

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

.SUFFIXES: .el .elc

.el.elc:
	$(BATCH) -f batch-byte-compile $<

deps:
	@for i in *.el tests/*.el; do \
		echo -n "$${i%.el}.elc:"; \
		grep -P "^ *\(require 'elfeed" $$i | tr "\n" " " | \
		sed "s#))* *#.el#g" | sed "s# *(require '# #g" | \
		sed "s#elfeed-[a-z-]*-tests.el#tests/\0#g"; \
		echo; \
	done > Makefile.deps

-include Makefile.deps
