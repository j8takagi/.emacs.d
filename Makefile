INSTALL = install
RSYNC := rsync
RSYNCFLAG := -avz --delete
EMACS := emacs
RMR := $(RM) -R
ECHO := echo
FIND := find

COMPILE.el := $(EMACS) -batch -f batch-byte-compile

init-files := $(wildcard init*.el)
site-lisp-files := $(shell $(FIND) site-lisp -name '*.el')

init-dir := ~/.emacs.d
site-lisp-dir := $(init-dir)/site-lisp

.PHONY: all install-init install-site-lisp puts_site-lisp-files puts_init-files

all: install-init install-site-lisp

install-init:
	install -d $(init-dir)
	$(RSYNC) $(RSYNCFLAG) $^ $(init-dir)/

install-site-lisp:
	install -d $(init-dir)
	$(RSYNC) $(RSYNCFLAG) site-lisp $(site-lisp-dir)/

clean:
	$(RMR) *.elc

%.elc: %.el
	$(COMPILE.el) $<

puts_init-files:
	@$(ECHO) $(init-files) $(addsuffix c,$(init-files))

puts_site-lisp-files:
	@$(ECHO) $(site-lisp-files) $(addsuffix c,$(site-lisp-files))
