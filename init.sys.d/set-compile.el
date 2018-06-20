(normal-top-level-add-subdirs-to-load-path)

(require 'package)
(package-initialize)

(require 'server)

(add-to-list 'load-path default-directory)
(add-to-list 'load-path "../site-lisp/listify")
(add-to-list 'load-path "../site-lisp/font-utilities")

(require 'listify)
(require 'fontset-set)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
