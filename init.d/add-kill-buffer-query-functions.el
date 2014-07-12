(defvar not-kill-but-bury-buffer-name-list
  '("*scratch*" "*Messages*")
  "List of buffer name, which is not killed but buried
when the buffer-kill is evaluted.")

(defun not-kill-but-bury ()
  (let ((bufname (buffer-name)))
    (if (not (member bufname not-kill-but-bury-buffer-name-list))
        t
      (bury-buffer)
      (message "%s is not killed, but buried." bufname)
      nil)))

(add-hook 'kill-buffer-query-functions 'not-kill-but-bury)
