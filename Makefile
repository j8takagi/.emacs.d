INSTALL = install

initdir = ~/.emacs.d
sitelispdir = /usr/local/share/emacs/site-lisp

.PHONY: install-init install-site-lisp

install-init:
	install -d $(initdir)
	install init*.el $(initdir)
	emacs -batch -f batch-byte-compile $(initdir)/init*.el

install-site-lisp:
	which $(sitelispdir) && cp site-lisp/* $(sitelispdir)/
