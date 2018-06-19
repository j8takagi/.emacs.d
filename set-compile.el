(normal-top-level-add-subdirs-to-load-path)

(require 'package)
(package-initialize)

(require 'server)

(add-to-list 'load-path default-directory)
(require 'listify-packages)
(require 'browse-kill-ring)
(require 'set-view-mode)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
