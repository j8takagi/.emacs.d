;;; no-process-query-on-exit.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'shell)

;; 引数で指定されたプロセスの名前が shell で子プロセスがない場合は、
;; process-query-on-exit-flag を nil に設定し、
;; "Buffer has a runnig process.; kill it?"
;; のプロンプト表示を抑制する。
(defun no-process-query-on-exit-set (proc)
  (when
      (and
       proc
       (equal (car (process-command proc)) shell-file-name)
       (null (process-running-child-p proc))
       )
    (set-process-query-on-exit-flag proc nil)))

(defun no-process-query-on-exit-set-current-buffer ()
  (no-process-query-on-exit-set
   (get-buffer-process (current-buffer))))

(defun no-process-query-on-exit-set-processes (&optional arg restart)
  (mapc
   (lambda (prop) (no-process-query-on-exit-set prop)) (process-list))
  (list arg restart))

(advice-add 'process-kill-buffer-query-function :before 'no-process-query-on-exit-set-current-buffer)

(advice-add 'save-buffers-kill-emacs :before 'no-process-query-on-exit-set-processes)

(provide 'no-process-query-on-exit)
;;; no-process-query-on-exit.el ends here
