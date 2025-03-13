EMACS ?= emacs

all: test

test: clean-elc
	${EMACS} -Q -batch -L . -l ob-aider.el -l ob-aider-tests.el -f ert-run-tests-batch-and-exit

lint: clean-elc
	${EMACS} -Q -batch --eval "(progn \
	(require 'package) \
	(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
	(package-initialize) \
	(unless (package-installed-p 'package-lint) \
	  (package-refresh-contents) \
	  (package-install 'package-lint)) \
	(require 'package-lint) \
	(setq byte-compile-error-on-warn t) \
	(find-file \"ob-aider.el\") \
	(package-lint-current-buffer) \
	(kill-emacs (if (boundp 'package-lint-errors) (length package-lint-errors) 0)))"

melpazoid: clean-elc
	chmod +x ./melpazoid-check.sh
	./melpazoid-check.sh

compile:
	${EMACS} -Q -batch -f batch-byte-compile ob-aider.el

clean-elc:
	rm -f *.elc

check: test lint melpazoid

.PHONY: all test lint melpazoid compile clean-elc check
