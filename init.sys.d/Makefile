EMACS := emacs
RSYNC := rsync
MKDIR := mkdir
GREPV := grep -v
RSYNCFLAG := -avz --delete

CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"
COMPILE.el := $(EMACS) -batch -l set-load-path.el -f batch-byte-compile

emacs-dir := ~/.emacs.d

KERNEL := $(shell uname)

ifeq ($(KERNEL),Linux)
  ELFILES := init-linux.el init-x.el
endif
ifeq ($(KERNEL),Darwin)
  ELFILES := init-mac.el
endif

ELCFILES := $(addsuffix c,$(ELFILES))

.PHONY: all install clean

all: $(ELCFILES)

install: all $(emacs-dir)/init.sys.d
	$(RSYNC) $(RSYNCFLAG) $(ELFILES) $(ELCFILES) $(emacs-dir)/init.sys.d/ | $(CLEAN.rsync)

$(emacs-dir)/init.sys.d:
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)
