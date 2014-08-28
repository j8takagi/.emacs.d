;; テンポラリバッファを開く
(defun switch-to-temp-buffer ()
  "Create temporary text-mode buffer."
  (interactive)
  ;; バッファ名は現在の日時
  (switch-to-buffer
   (generate-new-buffer
    (concat (replace-regexp-in-string " +" "_" (current-time-string)) ".txt")))
  ;; text-modeを設定
  (text-mode))

(provide 'temp-buffer)
