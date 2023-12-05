;;; amazon-url-simplify.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:

;;; Code:
(defun url-util-plus-encode-kill-ring ()
  "Encode URL string in kill-buffer."
  (interactive)
  (let (lastkill)
    (when (stringp (setq lastkill (car kill-ring)))
      (kill-new (url-encode-url lastkill))
      (message "Encoded URL is pushed to kill-ring."))))

(defun url-util-plus-amazon-url-simplify (url)
  "Simplify Amazon URL."
  (interactive "sURL: ")
  (when (string-match "\\(https://www.amazon.co.jp\\).*\\(/dp/[0-9X]+/?\\).*" url)
    (setq url (concat (match-string 1 url) (match-string 2 url)))))

(defun url-util-plus-amazon-url-simplify-kill-ring ()
  "Simplify Amazon URL string in kill-buffer."
  (interactive)
  (kill-new (url-util-plus-amazon-url-simplify (car kill-ring)))
  (message "Simplified Amazon URL is pushed to kill-ring."))

(provide 'amazon-url-simplify)
;;; amazon-url-simplify.el ends here
