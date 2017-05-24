;;; tidy-file-name-history.el --- 

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun delete-file-name-history-from-exclude-regexp ()
  "Delete file name matches `session-set-file-name-exclude-regexp' from `file-name-history'."
  (interactive)
  (let ((filenames file-name-history) afilename)
    (while
        (progn
          (setq afilename (car filenames))
          (when (string-match-p session-set-file-name-exclude-regexp afilename)
            (setq file-name-history (delete afilename file-name-history)))
          (setq filenames (cdr filenames))))
    file-name-history))

(defun delete-file-name-history-not-exist ()
  "Delete not existing file from `file-name-history'."
  (interactive)
  (let ((filenames file-name-history) afilename)
    (while
        (progn
          (setq afilename (car filenames))
          (unless (file-exists-p afilename)
            (setq file-name-history (delete afilename file-name-history)))
          (setq filenames (cdr filenames))))
    file-name-history))

(provide 'tidy-file-name-history)
;;; tidy-file-name-history.el ends here
