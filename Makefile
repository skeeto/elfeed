EMACS   ?= emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile
VERSION := $(word 1,$(subst -, ,$(shell git describe)))

EL  = elfeed.el
EL += xml-query.el
EL += elfeed-lib.el
EL += elfeed-log.el
EL += elfeed-db.el
EL += elfeed-search.el
EL += elfeed-show.el
EL += elfeed-curl.el
EL += elfeed-csv.el

ELC = $(EL:.el=.elc)

WEB_FILES  = web/elfeed-web.el
WEB_FILES += web/elfeed-web-pkg.el
WEB_FILES += web/index.html
WEB_FILES += web/elfeed.js
WEB_FILES += web/elfeed.css

EXTRA_DIST = README.md UNLICENSE

.PHONY : all package compile clean test

all : package

elfeed-$(VERSION).tar : $(EL) elfeed-pkg.el $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,elfeed-$(VERSION)/," $^

elfeed-web-$(VERSION).tar : $(WEB_FILES)
	tar -cf $@ --transform "s,^web/,elfeed-web-$(VERSION)/," $^

package: elfeed-$(VERSION).tar elfeed-web-$(VERSION).tar

test: compile
	$(BATCH) -L tests -l tests/elfeed-tests.el -f ert-run-tests-batch

compile: $(ELC)

clean:
	$(RM) *.tar *.elc

%.elc: %.el
	@$(COMPILE) $<
