(require 'cc-mode)

(define-skeleton c-if
  "if statement in c-mode."
  > "if(" _ ") {" n
  n "}")

(define-skeleton c-elseif
  "else if statement in c-mode."
  > "else if(" _ ") {" n
  n "}")

(define-skeleton c-else
  "else statement in c-mode."
  > "else {" n
  _ n "}")

(dolist (
         list
         '(
           ("if" c-if)
           ("elseif" c-elseif)
           ("else" c-else)
           ))
  (define-abbrev c-mode-abbrev-table (car list) "" (nth 1 list)))

(provide 'c-skeletons)
