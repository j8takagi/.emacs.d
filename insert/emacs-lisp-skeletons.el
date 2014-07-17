(define-skeleton emacs-lisp-template
  "template of emacs lisp file."
  nil
  ";;; " (file-name-nondirectory (buffer-file-name)) " --- " ?\n
  ?\n
  ";; Copyright (C) " (substring (current-time-string) -4) " by " user-login-name ?\n
  ?\n
  ";; Authors:" user-full-name ?\n
  ";; Keywords:" ?\n
  ?\n
  ";;; Commentary:" ?\n
  ?\n
  ?\n
  ";;; Code:" ?\n
  _ ?\n
  ?\n
  "(provide '" (file-name-base (buffer-file-name)) ")" ?\n
  ";;; " (file-name-nondirectory (buffer-file-name)) " ends here" ?\n
  )

(define-skeleton emacs-lisp-defun
  "define function statement in emacs-lisp."
  > "(defun " _ " ()" n
  "\"\"" n
  n
  ")" n
  n
  )

(define-skeleton emacs-lisp-dolist
  "dolist statement in emacs-lisp."
  > "(dolist" n
  > "(list" n
  > "'(" n
  > _ n
  > "))" n
  > "())" n
  )

;; Autoinserting
(define-auto-insert "\\.el\\'" [emacs-lisp-template])

;; Skeletons as Abbrev Expansions
(dolist (
         list
         '(
           ("dolist" emacs-lisp-dolist)
           ("defun" emacs-lisp-defun)
           ("template" emacs-lisp-template)
           ))
  (define-abbrev emacs-lisp-mode-abbrev-table (car list) "" (nth 1 list)))

(provide 'emacs-lisp-skeletons)
;;; emacs-lisp-skeletons.el ends here
