;;; js-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords: abbrev js-mode JavaScript skeleton

;;; Commentary:


;;; Code:
(require 'js)

(dolist (
         list
         '(
           ("if" js-if)
           ("elseif" js-elseif)
           ("else" js-else)
           ))
  (define-abbrev js-mode-abbrev-table (car list) "" (nth 1 list)))

(define-skeleton js-if
  "if statement in js-mode."
  > "if(" _ ") {" n
  n
  "}" n
  )

(define-skeleton js-elseif
  "else if statement in js-mode."
  > "else if(" _ ") {" n
  n
  "}" n
  )

(define-skeleton js-else
  "else statement in js-mode."
  > "else {" n
  > _ n
  > "}" n
  )

(provide 'js-skeletons)
;;; js-skeletons.el ends here

