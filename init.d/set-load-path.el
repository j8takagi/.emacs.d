(dolist
    (path
     '(
       "."
       "../init.sys.d"
       "../site-lisp"
       ))
  (let ((default-directory (expand-file-name path)))
    (add-to-list 'load-path default-directory)
    (when (file-exists-p "subdirs.el")
      (load-library "subdirs"))))

(require 'package)
(package-initialize)
