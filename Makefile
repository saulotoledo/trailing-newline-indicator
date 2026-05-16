EMACS ?= emacs

.PHONY: test
test:
	$(EMACS) --batch -Q \
	  -L . \
	  -l trailing-newline-indicator.el \
	  -l tests/trailing-newline-indicator-test.el \
	  -f ert-run-tests-batch-and-exit
