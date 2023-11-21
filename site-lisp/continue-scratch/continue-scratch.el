;;; continue-scratch.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: desktop scratch

;;; Commentary:


;;; Code:
(require 'desktop)

(defvar continue-scratch-buffer-string ""
  "*scratch* buffer string to be saved.")

(defvar continue-scratch-point 0
  "*scratch* buffer point to be saved.")

(defun continue-scratch-save()
  "Save current *Scratch* buffer
string to variable `continue-scratch-buffer-string'
and point to `continue-scratch-point', and
return list of buffer string and point."
  (with-current-buffer "*scratch*"
    (setq
     continue-scratch-buffer-string (buffer-string)
     continue-scratch-point (point)
     )
    (set-buffer-modified-p nil)
    )
  `(,continue-scratch-buffer-string ,continue-scratch-point))

(defun continue-scratch-load()
  "Load buffer string and point to *Scratch* buffer
from variable `continue-scratch-buffer-string' and
`continue-scratch-point'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((curr (buffer-string)))
      (when (member curr '("" initial-scratch-message))
        (unless (equal curr "")
          (erase-buffer))
        (insert continue-scratch-buffer-string)
        (goto-char continue-scratch-point)
        (set-buffer-modified-p nil)))))

(mapc
 (lambda (var)
   (unless (memq var desktop-globals-to-save)
     (push var desktop-globals-to-save)))
 '(continue-scratch-buffer-string continue-scratch-point))

(add-hook 'desktop-save-hook 'continue-scratch-save)
(add-hook 'desktop-after-read-hook 'continue-scratch-load)

(provide 'continue-scratch)
;;; continue-scratch.el ends here
