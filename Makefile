EMACS   ?= emacs
CASK    ?= cask
VIRTUAL := $(CASK) exec $(EMACS)
BATCH   := $(VIRTUAL) -Q -batch -L .
VERSION := $(word 1,$(subst -, ,$(shell git describe)))

PACKAGE := elfeed
VERSION := $(shell $(CASK) version)

EL  = elfeed.el
EL += xml-query.el
EL += elfeed-lib.el
EL += elfeed-sql.el
EL += elfeed-db.el
EL += elfeed-search.el
EL += elfeed-show.el

ELC = $(EL:.el=.elc)

WEB_FILES  = web/elfeed-web.el
WEB_FILES += web/elfeed-web-pkg.el
WEB_FILES += web/index.html
WEB_FILES += web/elfeed.js
WEB_FILES += web/elfeed.css

EXTRA_DIST = README.md UNLICENSE

.PHONY : all package compile clean test

all : test

.cask : Cask
	cask install
	touch .cask

$(PACKAGE)-pkg.el : Cask
	$(CASK) package

$(PACKAGE)-$(VERSION).tar : $(EL) $(PACKAGE)-pkg.el $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,$(PACKAGE)-$(VERSION)/," $^

$(PACKAGE)-web-$(VERSION).tar : $(WEB_FILES)
	tar -cf $@ --transform "s,^web/,$(PACKAGE)-web-$(VERSION)/," $^

package: $(PACKAGE)-$(VERSION).tar $(PACKAGE)-web-$(VERSION).tar

test: compile
	$(BATCH) -L tests -l tests/elfeed-tests.el -f ert-run-tests-batch

compile: .cask $(ELC)

clean:
	$(RM) *.tar *.elc

%.elc : %.el
	$(BATCH) -f batch-byte-compile $<
