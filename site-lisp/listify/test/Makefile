EMACS := emacs
EMACSFLAGS := -Q -batch -l test_update-or-add-alist.el
.PHONY: check test

check:
	@$(EMACS) $(EMACSFLAGS) -f ert-run-tests-batch-and-exit 2>&1 | tee result.log

test: check
