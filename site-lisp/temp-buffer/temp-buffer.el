;; テンポラリバッファを開く
(defun switch-to-temp-buffer ()
  "Create temporary buffer."
  (interactive)
  ;; バッファ名は現在の日時
  (switch-to-buffer
   (generate-new-buffer
    (concat "*" (replace-regexp-in-string " +" "_" (current-time-string)) "*"))
   (setq buffer-offer-save nil)))

(provide 'temp-buffer)
