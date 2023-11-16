;;; continue-scratch.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'desktop)

(defvar continue-scratch-buffer-string ""
  "*scratch* buffer string to be saved.")

(defvar continue-scratch-point 0
  "*scratch* buffer point to be saved.")

(defun continue-scratch-save()
  (with-current-buffer "*scratch*"
    (setq
     continue-scratch-buffer-string (buffer-string)
     continue-scratch-point (point)
     ))
  `(,continue-scratch-buffer-string ,continue-scratch-point))

(defun continue-scratch-load()
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((curr (buffer-string)))
      (when (member curr '("" initial-scratch-message))
        (unless (equal curr "")
          (erase-buffer))
        (insert continue-scratch-buffer-string)
        (goto-char continue-scratch-point)))))

(mapc
 (lambda (var)
   (unless (memq var desktop-globals-to-save)
     (push var desktop-globals-to-save)))
 '(continue-scratch-buffer-string continue-scratch-point))

(add-hook 'desktop-save-hook 'continue-scratch-save)

(add-hook 'desktop-after-read-hook 'continue-scratch-load)

(provide 'continue-scratch)
;;; continue-scratch.el ends here
