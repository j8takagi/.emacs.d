;;; shell-plus.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'shell)

;; シェルにカレントディレクトリへのcdコマンドを送る
(defun shell-plus-send-cd (dir)
  "Send cd command of default directory to shell."
  (interactive)
  (if (null (get-buffer "*shell*"))
      (message "*shell* buffer is not exist.")
    (with-current-buffer "*shell*"
      (unless (equal default-directory dir)
        (goto-char (point-max))
        (comint-kill-input)
        (insert (concat "cd '" (expand-file-name default-directory) "'"))
        (comint-send-input)
        (goto-char (point-max))))))

(defun shell-plus-shell-cd ()
   "If shell buffer exists, change directory in shell
to default directory in current buffer.
Otherwise, open new shell buffer of the dafault directory."
   (interactive)
   (let (abuf aproc)
     (when (not (and
                 (setq abuf (get-buffer "*shell*"))
                 (setq aproc (get-buffer-process abuf))))
       (shell))
     (if (process-running-child-p aproc)
         (message "Child process is running in the shell.")
       (switch-to-buffer "*shell*")
       (shell-plus-send-cd default-directory)
       (recenter 1))))

;; 引数で指定されたプロセスの名前が shell で子プロセスがない場合は、
;; process-query-on-exit-flag を nil に設定し、
;; "Buffer has a runnig process.; kill it?"
;; のプロンプト表示を抑制する。
(defun shell-plus-unset-query-on-exit-process (proc)
  (when
      (and
       proc
       (equal (car (process-command proc)) shell-file-name)
       (null (process-running-child-p proc))
       )
    (set-process-query-on-exit-flag proc nil)))

(defun shell-plus-unset-query-on-exit-buffer-process ()
  (shell-plus-unset-query-on-exit-process
   (get-buffer-process (current-buffer))))

(defun shell-plus-unset-query-on-exit-processes (&optional arg restart)
  (mapc
   (lambda (prop) (shell-plus-unset-query-on-exit-process prop)) (process-list))
  (list arg restart))

(define-minor-mode shell-plus-unset-query-on-exit-process-mode
  "Toggle unset query for process when Emacs is exitd or buffer is killed."
  :lighter nil
  (mapc
   (lambda (symfunc)
     (let (func args)
       (if shell-plus-unset-query-on-exit-process-mode
           (setq func 'advice-add args `(,(car symfunc) :before ,(cadr symfunc)))
         (setq func 'advice-remove args `(,(nth 0 symfunc) ,(nth 1 symfunc))))
       (apply func args)))
  '(
    (process-kill-buffer-query-function shell-plus-unset-query-on-exit-buffer-process)
    (save-buffers-kill-emacs shell-plus-unset-query-on-exit-processes)
    )))

(shell-plus-unset-query-on-exit-process-mode 1)
(provide 'shell-plus)
;;; shell-plus.el ends here
