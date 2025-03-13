EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${CASK} exec ${EMACS} -Q -batch -l ob-aider-tests.el -f ert-run-tests-batch-and-exit

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile ob-aider.el

clean-elc:
	rm -f *.elc

.PHONY: all test compile clean-elc
