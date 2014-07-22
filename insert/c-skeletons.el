;;; c-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'cc-mode)

(define-skeleton comment-javadoc-style
  "Skelton of JAVADoc style Comment."
  nil
  "/**" ?\n
  " * " _ ?\n
  "**/" ?\n
  )

(define-skeleton c-elseif
  "else if statement in c-mode."
  > "else if(" _ ") {" n
  n "}")

(define-skeleton c-else
  "else statement in c-mode."
  > "else {" n
  _ n "}")

(dolist
    (
     list
     '(
       ("/**" comment-javadoc-style)
       ("if" c-if)
       ("elseif" c-elseif)
       ("else" c-else)
       ))
  (let ((name (car list)) (hook (nth 1 list)))
    (define-abbrev c-mode-abbrev-table name "" hook)))

(provide 'c-skeletons)
;;; c-skeletons.el ends here
