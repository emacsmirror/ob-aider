EMACS ?= emacs

all: test

test: clean-elc
	${EMACS} -Q -batch -L . -l ob-aider.el -l ob-aider-tests.el -f ert-run-tests-batch-and-exit

compile:
	${EMACS} -Q -batch -f batch-byte-compile ob-aider.el

clean-elc:
	rm -f *.elc

.PHONY: all test compile clean-elc
