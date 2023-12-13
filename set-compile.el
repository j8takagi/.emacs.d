(normal-top-level-add-subdirs-to-load-path)

(require 'package)
(package-initialize)

(add-to-list 'load-path default-directory)
(mapcar
 (lambda (pkg) (require pkg))
 '(
   add-log
   asm-mode
   auto-elc-mode
   bison-mode
   cc-mode
   compile
   dired
   dired-aux
   ediff
   ess-site
   find-dired
   graphviz-dot-mode
   info
   lisp-mode
   listify
   listify-packages
   markdown-mode
   mpv-ts-mode
   nxml-mode
   server
   shell
   skeleton
   tex-mode
   undohist
   uniquify
   vc-hooks
   view-mode-init
   web-mode
   whitespace-init
   ))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
