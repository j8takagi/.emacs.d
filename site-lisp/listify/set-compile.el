(normal-top-level-add-subdirs-to-load-path)
(add-to-list 'load-path default-directory)

(require 'package)
(package-initialize)

(require 'server)


;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
