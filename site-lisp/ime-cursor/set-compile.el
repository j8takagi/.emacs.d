(normal-top-level-add-subdirs-to-load-path)

(require 'package)
(package-initialize)

(require 'server)

(add-to-list 'load-path default-directory)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
