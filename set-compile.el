(let ((default-directory (expand-file-name ".")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(package-initialize)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
