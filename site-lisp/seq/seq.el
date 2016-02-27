;;; seq.el ---

;; Copyright (C) 2016 by j8takagi

;; Authors: j8takagi
;; Keywords: 

;;; Commentary:

;;; Code:
(defun seq-add-beginning-of-line (from to &optional separator)
  "Add sequence to beginning of each line in the region."
  (interactive "r")
  (let ((num 0))
    (save-restriction
      (narrow-to-region from to)
      (save-excursion
        (goto-char (point-min))
        (unless separator
          (setq separator "\t"))
        (while (re-search-forward "^.+$" nil t)
          (replace-match (concat (number-to-string (setq num (+ 1 num))) separator "\\&")))))))

(provide 'seq)
;;; seq.el ends here
