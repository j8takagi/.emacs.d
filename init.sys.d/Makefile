EMACS := emacs
RSYNC := rsync
MKDIR := mkdir
GREPV := grep -v
RSYNCFLAG := -avz --delete

CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"
COMPILE.el := $(EMACS) -batch -l set-compile.el -f batch-byte-compile

emacs-dir := ~/.emacs.d

KERNEL := $(shell uname)
ifeq ($(KERNEL),Linux)
  OSFILES := $(wildcard init-linux*.el init-x*.el)
endif
ifeq ($(KERNEL),Darwin)
  OSFILES := $(wildcard init-mac*.el init-ns*.el init-darwin*.el)
endif

HOST := $(shell hostname -s)
HOSTFILES := $(wildcard init-$(HOST)*.el)

ELCFILES := $(addsuffix c,$(OSFILES) $(HOSTFILES))

.PHONY: all install clean

all: $(ELCFILES)

install: all $(emacs-dir)/init.sys.d
	$(RSYNC) $(RSYNCFLAG) $(OSFILES) $(HOSTFILES) $(ELCFILES) $(emacs-dir)/init.sys.d/ | $(CLEAN.rsync)

$(emacs-dir)/init.sys.d:
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)

distclean: clean
