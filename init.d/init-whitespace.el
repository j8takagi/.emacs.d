(require 'my-init)
(my-init-requires 'whitespace)

;タブ	、全角スペース　、行末の空白    
(my-init-custom-set-variables
 '(whitespace-style nil)
 '(whitespace-style (face tabs spaces trailing))
 '(whitespace-space-regexp "\\(　\\)")
 '(whitespace-trailing-regexp "\\( +$\\)")
 )

(let (
      (bg (cdr (assoc 'background-color (frame-parameters))))
      (fg (cdr (assoc 'foreground-color (frame-parameters)))))
  (custom-set-faces
   `(whitespace-space
     ((nil (
            :background ,bg
            :foreground ,fg
            :box (:line-width 2 :color "orange")
            ))))
  `(whitespace-tab
    ((nil (
           :background "white smoke"
           :foreground ,fg
           :box (:line-width 2 :color "navy")
           ))))
  `(whitespace-trailing
    ((nil (
           :background ,bg
           :foreground ,fg
           :underline "navy"
           ))))))

;; whitespaceを無効にするメジャーモード
(defcustom whitespace-disabled-major-mode-list nil
  "Major mode of disable whitespace-mode even if whitespace-mode is enabled"
  :type '(repeat symbol)
  :group 'whitespace)

;; メジャーモード設定後、バッファーが読み取り専用の場合と、
;; whitespaceを無効にするメジャーモードの場合以外、whitespaceを有効にする
(defun init-whitespace-mode ()
  "Enable whitespace mode unless the buffer is read only or the major mode is in the disabled list."
  (if (or buffer-read-only (member major-mode whitespace-disabled-major-mode-list))
      (whitespace-mode 0)
    (whitespace-mode 1)))

(my-init-custom-set-variables
 '(after-change-major-mode-hook (init-whitespace-mode))
 '(view-mode-hook (init-whitespace-mode)))

(provide 'init-whitespace)
