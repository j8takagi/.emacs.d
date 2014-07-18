;;; latex-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:

(define-skeleton latex-template
  "template of LaTeX file."
  nil
  "\\documentclass{jsarticle}" n
  "\\usepackage[dvipdfm,pdftitle={}]{hyperref}" n
  "\\usepackage{pxjahyper}" n
  "\\usepackage[dvipdfmx]{graphicx}" n
  "\\usepackage{amsmath,amssymb}" n
  "\\begin{document}" n

  "\\title{}" n
  "\\author{高木和人}" n
  "\\date{\\today}" n
  "\\maketitle" n
  n
  _ n
  n
  "%% \\bibliographystyle{jplain}" n
  "%% \\bibliography{ref}" n
  "\\end{document}" n)

(define-skeleton latex-table
  "table in latex."
  nil
  > "\begin{table}[htb]" n
  "\begin{tabular}{|l|c|r||r|} \hline" n
  _ "& & & \\ \hline" n
  "\end{tabular}" n
  "\end{table}" n)

;; Autoinserting
(define-auto-insert 'latex-mode [latex-template])

;; Skeletons as Abbrev Expansions
(dolist (
         list
         '(
           ("\table" latex-table)
           ))
  (define-abbrev latex-mode-abbrev-table (car list) "" (nth 1 list)))

(provide 'latex-skeletons)
;;; latex-skeletons.el ends here
