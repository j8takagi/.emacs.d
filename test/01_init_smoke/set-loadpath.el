;;; set-user-emacs-directory.el --- 

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun get-parent-directory (directory generaton)
  (if (not (directory-name-p directory))
      (error (format "%s is not a directory." directory))
    (let ((adir directory))
      (dotimes (i generaton)
        (setq adir (file-name-directory (directory-file-name adir))))
      adir)))


(let ((default-directory (get-parent-directory default-directory 2)))
  (normal-top-level-add-subdirs-to-load-path))

(provide 'set-user-emacs-directory)
;;; set-user-emacs-directory.el ends here
