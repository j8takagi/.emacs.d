;;; emacs-lisp-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:

;;; Code:
(require 'lisp-mode)

(define-skeleton emacs-lisp-template
  "Template of emacs lisp file."
  nil
  ";;; " (file-name-nondirectory (buffer-file-name)) " --- " ?\n
  ?\n
  ";; Copyright (C) " (substring (current-time-string) -4) " by " user-full-name ?\n
  ?\n
  ";; Authors: " user-full-name ?\n
  ";; Keywords: " ?\n
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
  > "(defun " _ " ()" ?\n
  "\"\"" ?\n
  ?\n
  ")" ?\n
  ?\n
  )

(define-skeleton emacs-lisp-defvar
  "define variable statement in emacs-lisp."
  > "(defvar " _ ?\n
  > "\"\")" ?\n
  ?\n
  )

(define-skeleton emacs-lisp-dolist
  "dolist statement in emacs-lisp."
  > "(dolist" ?\n
  > "(list" ?\n
  > "'(" ?\n
  > _ ?\n
  > "))" ?\n
  > "())" ?\n
  )

;; Autoinserting
(define-auto-insert "\\.el\\'" 'emacs-lisp-template)

(provide 'emacs-lisp-skeletons)
;;; emacs-lisp-skeletons.el ends here
