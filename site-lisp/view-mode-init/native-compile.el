(defun init-native-compile ()
  (when (comp-native-compiler-options-effective-p)
    (mapc (lambda (file) (native-compile file)) command-line-args-left)))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
