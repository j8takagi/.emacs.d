;; 引数で指定されたプロセスの名前が shell で子プロセスがない場合は、
;; process-query-on-exit-flag を nil に設定し、
;; "Buffer has a runnig process.; kill it?"
;; のプロンプト表示を抑制する。
(defun set-process-not-running-child-noquery-on-exit (proc)
  (when (and proc (string= (process-name proc) "shell"))
    (set-process-query-on-exit-flag proc (process-running-child-p proc))))

(defadvice kill-buffer (before my-set-process-query activate)
  (set-process-not-running-child-noquery-on-exit (get-buffer-process (current-buffer))))

(defadvice save-buffers-kill-terminal (before my-set-process-query activate)
  (dolist (proc (process-list))
    (set-process-not-running-child-noquery-on-exit proc)))
