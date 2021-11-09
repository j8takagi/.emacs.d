;;; latex-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'tex-mode)

(define-skeleton latex-template
  "template of LaTeX file."
  nil
  "\\documentclass[11pt, a4paper, uplatex]{jsarticle}" n
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

(define-skeleton latex-section
  "section in latex."
  nil
  "\\section{" _ "}"
  )

(define-skeleton latex-subsection
  "subsection in latex."
  nil
  "\\subsection{" _ "}"
  )

(define-skeleton latex-cite
  "cite in latex."
  nil
  "\\cite{" _ "}"
  )

(define-skeleton latex-table
  "table in latex."
  nil
  > "\\begin{table}[htb]" ?\n
  "\\begin{tabular}{|l|c|r||r|} \\hline" ?\n
  _ "& & & \\\\ \\hline" ?\n
  "\\end{tabular}" ?\n
  "\\end{table}" ?\n)

(define-skeleton latex-figure
  "figure in latex."
  nil
  "\\begin{figure}" ?\n
  > "\\centering" ?\n
  > "\\caption{" _ "}" ?\n
  > "\\label{fig:}" ?\n
  > "\\includegraphics[height=cm]{}" ?\n
  "\\end{figure}" ?\n
  )

(define-skeleton latex-itemize
  "itemize in latex."
  nil
  > "\\begin{itemize}" ?\n
  "\\item " _ ?\n
  "\\end{itemize}" ?\n
  )

(define-skeleton latex-enumerate
  "enumerate in latex."
  nil
  > "\\begin{enumerate}" n
  "\\item " _ n
  "\\end{enumerate}"
  )

(define-skeleton latex-quote
  "quote in latex."
  nil
  > "\\begin{quote}" n
  _ n
  "\\end{quote}"
  )

(define-skeleton latex-quotation
  "quotation in latex."
  nil
  > "\\begin{quotation}" n
  _ n
  "\\end{quotation}"
  )

(define-skeleton latex-equation
  "equation in latex."
  nil
  > "\\begin{equation}" n
  _ n
  "\\end{equation}"
  )

(define-skeleton latex-eqnarray
  "eqnarray in latex."
  nil
  > "\\begin{eqnarray}" n
  _ n
  "\\end{eqnarray}"
  )

(define-skeleton latex-equation-nonumber
  "equation in latex."
  nil
  > "\\[" n
  _ n
  "\\]"
  )

(define-skeleton latex-eqnarray-nonumber
  "eqnarray* in latex."
  nil
  > "\\begin{eqnarray*}" n
  _ n
  "\\end{eqnarray*}"
  )

;; Skeletons as Abbrev Expansions
(dolist (
         list
         '(
           ("section" latex-section)
           ("subsection" latex-subsection)
           ("cite" latex-cite)
           ("table" latex-table)
           ("figure" latex-figure)
           ("itemize" latex-itemize)
           ("enumerate" latex-enumerate)
           ("quote" latex-quote)
           ("quotation" latex-quotation)
           ("eq" latex-equation)
           ("eqs" latex-eqnarray)
           ("eqn" latex-equation-nonumber)
           ("eqsn" latex-eqnarray-nonumber)
           ))
  (let ((name (car list)) (hook (nth 1 list)))
    (define-abbrev latex-mode-abbrev-table name "" hook)))

(provide 'latex-skeletons)
;;; latex-skeletons.el ends here
