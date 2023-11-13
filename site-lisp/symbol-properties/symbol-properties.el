;;; symbol-properties.el --- get symbol properties -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun symbol-properties (sym)
  "Return the list of SYMBOL's PROPNAMEs."
  (let ((props nil) (prop (symbol-plist sym)) (i 0))
    (while (< i (length prop))
      (when (= (% i 2) 0)
        (push (nth i prop) props))
      (setq i (+ i 1)))
    props
    ))

(provide 'symbol-properties)
;;; symbol-properties.el ends here
