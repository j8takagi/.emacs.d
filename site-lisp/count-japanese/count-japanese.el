;; 日本語を数える
(defun count-japanese ()
  (interactive)
  (message "日本語の文字数: %d字" (how-many "\\cj" (point-min) (point-max))))

(provide 'count-japanese)
