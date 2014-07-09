(setq whitespace-style '(face tabs spaces trailing)) ;  タブ

(setq whitespace-space-regexp "\\(　\\)")     ;　全角スペース

(setq whitespace-trailing-regexp "\\( +$\\)") ;行末の空白    


(set-face-attribute whitespace-tab nil :box "navy" :background (background-color-at-point))

(set-face-attribute whitespace-space nil :box "orange" :background (background-color-at-point))

(set-face-attribute whitespace-trailing nil
                    :foreground "navy" :background (background-color-at-point) :underline "navy")

;; whitespaceを無効にするメジャーモード
(defvar whitespace-disabled-major-mode-list
  "Major mode of disable whitespace-mode even if whitespace-mode is enabled")

(setq whitespace-disabled-major-mode-list
      '(mew-summary-mode completion-list-mode help-mode
        magit-mode tetris-mode w3m-mode mew-message-mode))

;; メジャーモード設定後、バッファーが読み取り専用でない場合はwhitespaceを有効にする
(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (unless (or buffer-read-only (member major-mode whitespace-disabled-major-mode-list))
               (whitespace-mode 1))))
