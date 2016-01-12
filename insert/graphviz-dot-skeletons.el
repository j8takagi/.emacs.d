;;; graphviz-dot-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:

(define-skeleton graphviz-dot-template
  "template of graphviz-dot file."
  nil
  "digraph sample {" ?\n
  > "graph [shape=\"\" style=\"\" color=\"\" fillcolor=\"\" fontname=\"sans-serif\" fontsize=\"12\" width=\"2\"];" ?\n
  > _ " -> " ?\n
  "}" ?\n
  ?\n
  )

(provide 'graphviz-dot-skeletons)
;;; graphviz-dot-skeletons.el ends here
