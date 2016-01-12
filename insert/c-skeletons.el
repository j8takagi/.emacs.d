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

(define-skeleton c-if
  "if statement in c-mode."
  > "if(" _ ") {" ?\n
  > ?\n
  > "}" ?\n
  )

(define-skeleton c-elseif
  "else if statement in c-mode."
  > "else if(" _ ") {" ?\n
  > ?\n
  > "}" ?\n
  )

(define-skeleton c-else
  "else statement in c-mode."
  > "else {" ?\n
  > _ ?\n
  > "}" ?\n
  )

(provide 'c-skeletons)
;;; c-skeletons.el ends here
