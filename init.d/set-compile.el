(let ((default-directory (expand-file-name "..")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(package-initialize)
