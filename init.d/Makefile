EMACS := emacs
RSYNC := rsync
MAKEDIR := mkdir
GREPV := grep -v

RSYNCFLAG := -avz --delete

CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"
COMPILE.el := $(EMACS) -batch -l set-load-path.el -f batch-byte-compile

ELFILES := $(wildcard *.el)
ELCFILES := $(addsuffix c,$(ELFILES))

emacs-dir := ~/.emacs.d

.PHONY: all install clean

all: $(ELCFILES)

install: all $(emacs-dir)/init.d
	$(RSYNC) $(RSYNCFLAG) --exclude='set-load-path.el*' $(ELFILES) $(ELCFILES) $(emacs-dir)/init.d/ | $(CLEAN.rsync)

$(emacs-dir)/init.d:
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)
