;; 1行上へスクロール
(defun scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

;; 1行下へスクロール
(defun scroll-down-one-line ()
  (interactive)
  (scroll-down 1))

(provide 'scroll-one-line)
