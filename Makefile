EMACS ?= emacs

all: test

test: clean-elc
	${EMACS} -Q -batch -L . -l ob-aider.el -l ob-aider-tests.el -f ert-run-tests-batch-and-exit

lint: clean-elc
	${EMACS} -Q -batch -f package-lint-batch-and-exit ob-aider.el

melpazoid: clean-elc
	chmod +x ./melpazoid-check.sh
	./melpazoid-check.sh

compile:
	${EMACS} -Q -batch -f batch-byte-compile ob-aider.el

clean-elc:
	rm -f *.elc

check: test lint melpazoid

.PHONY: all test lint melpazoid compile clean-elc check
