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

(define-skeleton c-for
  "for statement in c-mode."
  > "for(" _ "; ; ) {" ?\n
  > ?\n
  > "}" ?\n
  )

(define-skeleton c-while
  "while statement in c-mode."
  > "while(" _ ") {" ?\n
  > ?\n
  > "}" ?\n
  )

(define-skeleton c-dowhile
  "do while statement in c-mode."
  > "do {" ?\n
  >  _ ?\n
  > "} while( );" ?\n
  )

(define-skeleton c-switch
  "switch statement in c-mode."
  > "switch(" _ ") {" ?\n
  > "case :" ?\n
  >  ?\n
  > "break;" ?\n
  > "default:" ?\n
  >  ?\n
  > "break;" ?\n
  > "}" ?\n
  )

(define-skeleton c-case
  "case statement of switch in c-mode."
  > "case :" ?\n
  >  ?\n
  > "break;" ?\n
  )

(provide 'c-skeletons)
;;; c-skeletons.el ends here
