;;; tsv-mode.el ---

;; Copyright (C) 2016 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'csv-mode)

(define-derived-mode tsv-mode csv-mode "TSV"
  "Major mode for editing files tab-separated value type."
  (set (make-local-variable 'csv-separators) '("\t")))

(provide 'tsv-mode)
;;; tsv-mode.el ends here
