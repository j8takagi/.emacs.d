;;; symbol-properties.el --- get symbol properties -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: variable property

;;; Commentary:


;;; Code:
(defun symbol-properties (sym)
  "Return the list of SYMBOL's PROPNAMEs."
  (let ((propnames nil) (prop (symbol-plist sym)))
    (dolist (i (number-sequence 0 (- (length prop) 1)))
      (when (= (% i 2) 0)
        (push (nth i prop) propnames)))
    (reverse propnames)))

(provide 'symbol-properties)
;;; symbol-properties.el ends here
