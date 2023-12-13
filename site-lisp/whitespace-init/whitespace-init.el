;;; whitespace-init.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'whitespace)

;; whitespaceを無効にするメジャーモード
(defcustom whitespace-init-disabled-major-mode-list nil
  "Major mode of disable whitespace-mode even if whitespace-mode is enabled"
  :type '(repeat symbol)
  :group 'whitespace
  )

;; メジャーモード設定後、バッファーが読み取り専用の場合と、
;; whitespaceを無効にするメジャーモードの場合以外、whitespaceを有効にする
(defun whitespace-init-enable-mode ()
  "Enable whitespace mode unless the buffer is read only or the major
mode is in the disabled list."
  (if (or buffer-read-only (member major-mode whitespace-init-disabled-major-mode-list))
      (whitespace-mode 0)
    (whitespace-mode 1)))

(defun whitespace-init-tabs-spaces-trailing ()
  "Set display on whitespace mode of tabs	, zenkaku spaces　 and trailing.    
And other display settings on whitespace mode are unset."
  (custom-set-variables
   '(whitespace-style '(face tabs spaces trailing))
   '(whitespace-space-regexp "\\(　\\)")
   '(whitespace-trailing-regexp "\\( +$\\)")
   )
  (custom-set-faces
   '(whitespace-space ((nil ( :box (:line-width 2 :color "orange")))))
   '(whitespace-tab ((nil (:background "white smoke" :box (:line-width 2 :color "navy")))))
   '(whitespace-trailing ((nil (:underline "navy"))))
   )
  t
  )

(provide 'whitespace-init)
;;; whitespace-init.el ends here
