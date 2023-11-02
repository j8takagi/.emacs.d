;;; revert-dired-buffers.el --- 

;; Copyright (C) 2018 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary: shell-command後、diredバッファを自動更新する


;;; Code:

(defun revert-dired-buffers (&optional command output-buffer error-buffer)
  (interactive)
  (dolist (b (buffer-list))
    (set-buffer b)
    (when (eq major-mode 'dired-mode)
      (condition-case err
            (revert-buffer nil 1)))))

(advice-add 'shell-command :after 'revert-dired-buffers)
(advice-add 'switch-to-buffer :after 'revert-dired-buffers)

;(advice-add 'dired-mode :after 'auto-revert-mode)

(provide 'revert-dired-buffers)
;;; revert-dired-buffers.el ends here
