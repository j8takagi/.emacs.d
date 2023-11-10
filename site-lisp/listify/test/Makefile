TARGETS := test_listify-set-alists test_listify-set test_update-or-add-alist
EMACS := emacs
EMACSFLAGS := -Q -batch
.PHONY: check test %.log

check: $(addsuffix .log,$(TARGETS))

test: check

clean:
	$(RM) *.elc *.log

%.log: %.el
	$(EMACS) $(EMACSFLAGS) -l $< -f ert-run-tests-batch-and-exit 2>&1 >$@
