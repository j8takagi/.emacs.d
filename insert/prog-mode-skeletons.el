;;; prog-mode-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(define-skeleton comment-javadoc-style
  "Skelton of JAVADoc style Comment."
  nil
  "/**" ?\n
  " * " _ ?\n
  "**/" ?\n
  )

;; Skeletons as Abbrev Expansions
(dolist (
         list
         '(
           ("/**" comment-javadoc-style)
           ))
  (define-abbrev prog-mode-abbrev-table (car list) "" (nth 1 list)))

(provide 'prog-mode-skeletons)
;;; prog-mode-skeletons.el ends here

