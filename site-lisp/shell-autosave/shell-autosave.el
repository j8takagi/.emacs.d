;;; shell-autosave.el --- 

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'shell)

(defun shell-autosave (file)
  (switch-to-buffer (find-file-noselect file))
  (shell (current-buffer))
  (run-with-idle-timer 1 t (current-buffer)))

(defun shell-autosave-unique-filename (file)
  (let ((anum 0))
    (while (file-exists-p file)
      (setq anum (+ anum 1))
      (setq file
            (concat
             (file-name-sans-extension file)
             (number-to-string anum) "."
             (file-name-extension file)
              ))))
  file)

(defun shell-autosave-typescript (&optional dir)
    (interactive "P")
    (if (null dir)
        (setq dir default-directory)
      (setq dir (read-directory-name "Enter directory: ")))
    (if (null (file-directory-p dir))
        (message "Invalid directory")
      (let ((afile (concat dir "typescript.log")))
        (shell-autosave afile)
        (read-only-mode -1))))

(provide 'shell-autosave)
;;; shell-autosave.el ends here
