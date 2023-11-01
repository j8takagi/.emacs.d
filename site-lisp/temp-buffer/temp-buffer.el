;; テンポラリバッファを開く
(defun switch-to-temp-buffer ()
  "Create temporary text-mode buffer."
  (interactive)
  ;; バッファ名は現在の日時
  (switch-to-buffer
   (generate-new-buffer (format-time-string "*temporary buffer %m-%d %H:%M*")))
  ;; text-modeを設定
  (text-mode))

(provide 'temp-buffer)
