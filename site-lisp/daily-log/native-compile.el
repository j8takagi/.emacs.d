(defun init-native-compile ()
  (mapc (lambda (file) (native-compile file)) command-line-args-left))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
