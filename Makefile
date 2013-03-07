INSTALL = install
RSYNC := rsync
RSYNCFLAG := -az --delete
EMACS := emacs
RMR := $(RM) -R
ECHO := echo
FIND := find

COMPILE.el := $(EMACS) -batch -script ~/.emacs.d/init.el -f batch-byte-compile
KERNEL := $(shell uname)

ifeq ($(KERNEL),Linux)
  init-files := init.el init-linux.el init-x.el
endif
ifeq ($(KERNEL),Darwin)
  init-files := init.el init-linux.el init-ubuntu-x.el
endif

site-lisp-files := $(shell $(FIND) site-lisp -name '*.el')

init-dir := ~/.emacs.d
site-lisp-dir := $(init-dir)/site-lisp

.PHONY: all install-init install-site-lisp puts_site-lisp-files puts_init-files

all: $(addsuffix c,$(init-files))

$(addsuffix c,$(init-files)): $(init-files)

install: install-init install-site-lisp

install-init: all
	install -d $(init-dir)
	$(RSYNC) $(RSYNCFLAG) $(init-files) $(addsuffix c,$(init-files)) $(init-dir)/

install-site-lisp:
	install -d $(init-dir)
	$(RSYNC) $(RSYNCFLAG) site-lisp/ $(site-lisp-dir)/

clean:
	$(RMR) *.elc

%.elc: %.el
	$(COMPILE.el) $<

puts_init-files:
	@$(ECHO) $(init-files) $(addsuffix c,$(init-files))

puts_site-lisp-files:
	@$(ECHO) $(site-lisp-files) $(addsuffix c,$(site-lisp-files))
