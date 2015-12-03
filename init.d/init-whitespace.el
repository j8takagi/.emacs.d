(require 'whitespace)

(setq whitespace-style '(face tabs spaces trailing)) ;	タブ
(setq whitespace-space-regexp "\\(　\\)")     ;　全角スペース
(setq whitespace-trailing-regexp "\\( +$\\)") ;行末の空白    

(defun init-whitespace-set-space ()
  (let ((bg (cdr (assoc 'background-color (frame-parameters)))))
    (set-face-attribute 'whitespace-tab nil
                        :box "navy" :background bg)
    (set-face-attribute 'whitespace-space nil
                        :box "orange" :background bg)
    (set-face-attribute 'whitespace-trailing nil
                        :foreground "navy" :background bg :underline "navy")))

(init-whitespace-set-space)

;; whitespaceを無効にするメジャーモード
(defvar whitespace-disabled-major-mode-list
  "Major mode of disable whitespace-mode even if whitespace-mode is enabled")

(setq whitespace-disabled-major-mode-list
      '(mew-summary-mode completion-list-mode help-mode
        magit-mode tetris-mode w3m-mode mew-message-mode))

;; メジャーモード設定後、バッファーが読み取り専用の場合と、
;; whitespaceを無効にするメジャーモードの場合以外、whitespaceを有効にする
(defun init-whitespace-enable ()
  "Enable whitespace mode unless the buffer is read only or the major mode is in the disabled list."
  (unless
      (or buffer-read-only
          (member major-mode whitespace-disabled-major-mode-list))
    (init-whitespace-set-space)
    (whitespace-mode 1)))

(add-hook 'after-change-major-mode-hook 'init-whitespace-enable)

(provide 'init-whitespace)
