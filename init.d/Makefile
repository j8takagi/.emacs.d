EMACS := emacs
RSYNC := rsync
MAKEDIR := mkdir

RSYNCFLAG := -avz --delete
COMPILE.el := $(EMACS) -batch -f batch-byte-compile

ELFILES := $(wildcard *.el)
ELCFILES := $(addsuffix c,$(ELFILES))

emacs-dir := ~/.emacs.d

.PHONY: all install clean

all: $(ELCFILES)

install: all $(emacs-dir)/init.d
	$(RSYNC) $(RSYNCFLAG) $(ELFILES) $(ELCFILES) $(emacs-dir)/init.d/

$(emacs-dir)/init.d:
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)
