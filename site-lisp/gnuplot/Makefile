EMACS := emacs

COMPILE.el := $(EMACS) -batch -f batch-byte-compile

ELFILES := $(wildcard *.el)
ELCFILES := $(addsuffix c,$(ELFILES))

.PHONY: all install clean

all: $(ELCFILES)

install: all

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)
