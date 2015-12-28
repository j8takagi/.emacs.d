;;;###autoload
(defun scroll-up-one-line ()
  "Scroll text of selected window upward 1 line."
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun scroll-down-one-line ()
  "Scroll text of selected window down 1 line."
  (interactive)
  (scroll-down 1))

(provide 'scroll-one-line)
