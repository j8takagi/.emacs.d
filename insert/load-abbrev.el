(let ((abbrevfile "~/.emacs.d/abbrev_defs"))
  (when (file-exists-p abbrevfile)
    (load-file "~/.emacs.d/abbrev_defs")))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
