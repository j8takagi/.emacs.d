EMACS := emacs
RSYNC := rsync
MKDIR := mkdir

RSYNCFLAG := -avz --delete
COMPILE.el := $(EMACS) -batch -f batch-byte-compile

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
	$(RSYNC) $(RSYNCFLAG) $(ELFILES) $(ELCFILES) $(emacs-dir)/init.sys.d/

$(emacs-dir)/init.sys.d:
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)
