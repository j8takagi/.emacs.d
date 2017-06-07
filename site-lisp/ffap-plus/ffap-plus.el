;;; ffap-plus.el --- 

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun find-file-at-point-immediate ()
  (interactive)
  (let (afile)
    (when (setq afile (ffap-file-at-point))
      (find-file afile))))

(provide 'ffap-plus)
;;; ffap-plus.el ends here
