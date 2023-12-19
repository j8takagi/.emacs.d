;;; bury-always-buffer.el -*- lexical-binding: t -*-
(defgroup bury-always-buffer nil
  "Set a buffer always not to kill, but to bury
when buffer-kill is called."
  :group 'convinience
  )

(defcustom bury-always-buffer-list
  '("*scratch*" "*Messages*")
  "List of buffer name, which is not killed but buried
when buffer-kill is called."
  :type '(repeat (string :tag "Buffer name"))
  :group 'bury-always-buffer
  )

(defun bury-always-buffer ()
  "Set a buffer not to kill, but to bury."
  (let ((res t) (bufname (buffer-name)))
    (when (member bufname bury-always-buffer-list)
      (bury-buffer)
      (message "%s is not killed, but buried." bufname)
      (setq res nil))
    res))

(add-hook 'kill-buffer-query-functions 'bury-always-buffer)

(provide 'bury-always-buffer)
