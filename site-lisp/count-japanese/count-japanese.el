;; 日本語を数える
(defun count-japanese-do (start end)
  (message "日本語の文字数: %d字" (how-many "\\cj" start end)))

(defun count-japanese (start end &optional arg)
  "Count the number of Japanese characters in the region.
If called interactively, print a message reporting the number of
Japanese characters in the region (whether or not the
region is active); with prefix ARG, report for the entire buffer
rather than the region."
  (interactive (if current-prefix-arg
           (list nil nil current-prefix-arg)
         (list (region-beginning) (region-end) nil)))
  (cond
   ((not (called-interactively-p 'any))
    (count-japanese-do start end))
   (arg
    (count-japanese-do (point-min) (point-max)))
   (t
    (count-japanese-do start end))))

(provide 'count-japanese)
