EMACS := /usr/local/bin/emacs
EMACSFLAGS := -Q -batch -l cus-edit -l *.el
.PHONY: check test

check:
	@$(EMACS) $(EMACSFLAGS) -f ert-run-tests-batch-and-exit 2>&1 | tee result.log

test: check
