(require 'view)

;; **scratch* と Messages* バッファーに、not-kill-but-buryを適用
(defvar not-kill-but-bury-buffer-name-list
  '("*scratch*" "*Messages*")
  "List of buffer name, which is not killed but buried
when the buffer-kill is evaluted.")

(defun not-kill-but-bury-buffer ()
  (let ((bufname (buffer-name)))
    (if (not (member bufname not-kill-but-bury-buffer-name-list))
        t
      (bury-buffer)
      (message "%s is not killed, but buried." bufname)
      nil)))

(add-hook 'kill-buffer-query-functions 'not-kill-but-bury-buffer)

;; *Messages* バッファーを view-mode に
(eval-after-load "view"
  (save-current-buffer
    (progn
      (set-buffer "*Messages*")
      (view-mode))))

(provide 'init-scratch-messages)
