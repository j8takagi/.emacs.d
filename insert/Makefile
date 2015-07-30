EMACS := emacs
MKDIR := mkdir -p
RSYNC := rsync
RSYNCFLAG := -avz --delete
GREPV := grep -v

CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"
COMPILE.el := $(EMACS) -batch -l set-load-path.el -l load-abbrev -f batch-byte-compile

emacs-dir := ~/.emacs.d

all: $(addsuffix c,$(wildcard *.el))

install: all $(emacs-dir)/insert
	$(RSYNC) $(RSYNCFLAG) --exclude='set-load-path.el*' --exclude='load-abbrev.el*' *.el *.elc $(emacs-dir)/insert/ | $(CLEAN.rsync)

$(emacs-dir)/insert:
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) *.elc
