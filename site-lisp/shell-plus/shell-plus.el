;;; shell-plus.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'shell)
(define-minor-mode shell-plus-ssh-track-mode
  "Toggle SSH tracking in this shell buffer (SSH track mode)."
  :lighter nil
  (setq list-buffers-directory (when shell-plus-ssh-track-mode default-directory))
  (if shell-plus-ssh-track-mode
      (add-hook 'comint-output-filter-functions 'shell-plus-set-default-directory)
    (remove-hook 'comint-output-filter-functions 'shell-plus-set-default-directory)))

(defvar-local shell-plus-ssh-pid-stack nil)

(defun shell-plus-get-ssh-directory (str)
  (let (args aserver auser aport)
    (when str
      (setq str (shell-plus-rm-trailing-newline str))
      (set-text-properties 0 (length str) nil str)
      (when (string-match "\\`\\(.+/\\)?\\(ssh\\)[ \t]+\\(.*\\)?" str)
        (when (and
               (setq args (match-string 3 str))
               (string-match "\\`\\(.*\\)-p[ \t]*\\([0-9]+\\)\\(.*\\)\\'" args))
          (setq
           aport (match-string 2 args)
           args (concat (match-string 1 args) (match-string 3 args))))
        (when (string-match "\\`\\(.*\\)-l[ \t]+\\([^ \t\n]+\\)\\(.*\\)\\'" args)
          (setq
           auser (match-string 2 args)
           args (concat (match-string 1 args) (match-string 3 args))))
        (when (string-match "\\(.*[ \t]+\\)?\\([a-zA-Z0-9-]+@\\)?\\([.a-zA-Z0-9-]+\\)\\'" args)
          (setq aserver (match-string 3 args))
          (when (match-string 2 args)
            (setq auser
                  (replace-regexp-in-string "@\\'" "" (match-string 2 args)))))
        (when aserver
          (concat
           "/ssh:"
           (when auser (concat auser "@"))
           aserver
           (when aport (concat "#" aport))
           ":" ))))))

(defun shell-plus-get-ssh-pid (&optional process directory)
  "Return running ssh process id in child processes of PROCESS.
If PROCESS is omitted or nil, child processes of current buffer process."
  (unless (and directory (file-directory-p directory))
    (setq directory "/"))
  (let (ppid match-pid (default-directory directory))
    (unless process
      (setq process (get-buffer-process (current-buffer))))
    (setq ppid (process-id process))
    (unless
        (catch 'found
          (dolist (pid (list-system-processes))
            (when
                (and
                 (equal ppid (cdr (assoc 'ppid (process-attributes pid))))
                 (member (cdr (assoc 'state (process-attributes pid))) '("R" "S"))
                 (equal "ssh" (cdr (assoc 'comm (process-attributes pid))))
                 )
              (setq match-pid pid)
              (throw 'found t)))))
    match-pid))

(defun shell-plus-set-default-directory (&optional str)
  (interactive)
  (ignore str)
  (let ((buf-proc (get-buffer-process (current-buffer))) ssh-pid sshspec)
    (if (null (setq ssh-pid (shell-plus-get-ssh-pid buf-proc)))
        (setq
           shell-plus-ssh-pid-stack nil
           comint-file-name-prefix nil)
      (when (or
             (null shell-plus-ssh-pid-stack)
             (not (equal ssh-pid (car shell-plus-ssh-pid-stack))))
        (setq
         sshspec
         (shell-plus-get-ssh-directory
          (cdr (assoc 'args (process-attributes ssh-pid)))))
        (when sshspec
          (push ssh-pid shell-plus-ssh-pid-stack)
          (setq comint-file-name-prefix sshspec))))
    (shell-resync-dirs)))

(defun shell-plus-rm-trailing-newline (str)
  (replace-regexp-in-string "[ \t\r\n]+\\'" "" str)
    )

(defun shell-plus-add-trailing-newline (str)
  (unless (string-match "\n``'" str)
    (concat str "\n")))

(defun shell-plus-eval-command (process cmdstr)
  "Eval command CMDSTR in the process and return the result."
  (let* (
         (normal-filter (process-filter process))
         (normal-buffer (process-buffer process))
         (result "")
         (prev nil)
         )
    (unless (equal (process-status process) 'run)
      (error "Status of process `%s' is `%s', not `run'."
             process (process-status process)))
    (unwind-protect
        (progn
          (set-process-filter
           process
           (lambda (_proc string)
             (setq result (concat result string))))
          (set-process-buffer process nil)
          (process-send-string process (shell-plus-add-trailing-newline cmdstr))
          ;; Wait until we get a prompt (which will be a line without
          ;; a newline).  This is far from fool-proof -- if something
          ;; outputs incomplete data and then sleeps, we'll think
          ;; we've received the prompt.
          (while (not (let* ((lines (string-lines result))
                             (last (car (last lines))))
                        (and (length> lines 0)
                             (not (equal last ""))
                             (or (not prev)
                                 (not (equal last prev)))
                             (setq prev last))))
            (accept-process-output process 0.2)))
      ;; Restore buffer.
      (set-process-buffer process normal-buffer)
      ;; Restore old filter.
      (set-process-filter process normal-filter))
    ;; Remove the prompt.
    (setq result (replace-regexp-in-string "\n.*\\'" "" result))
    (if (string-match "\n" result)
        (shell-plus-add-trailing-newline result)
      result)))

(defun shell-plus-send-cmd (cmdstr &optional shellbuf)
  (let (proc)
    (when (null shellbuf)
      (setq shellbuf (current-buffer)))
    (unless (setq proc (get-buffer-process shellbuf))
      (error "Process of buffer `%s' does not exist." (buffer-name shellbuf)))
    (shell-plus-eval-command proc cmdstr)))

(defun shell-plus-get-hostname (&optional shellbuf)
  (shell-plus-send-cmd "hostname" shellbuf))

(defun shell-plus-get-exit-status (&optional shellbuf)
  (shell-plus-send-cmd "echo $?" shellbuf))

(defun shell-plus-get-pwd (&optional shellbuf)
  (shell-plus-send-cmd "pwd" shellbuf))

(defun shell-plus-shell-local-p (&optional shellbuf)
  (equal (shell-plus-get-hostname shellbuf) (system-name)))

(defun shell-plus-send-cd (&optional dir shellbuf)
  "Send cd command of default directory to shell."
  (interactive)
  (when (null dir)
    (setq dir default-directory))
  (unless (file-directory-p dir)
    (error "Directory `%s' does not exist." dir))
  (when (null shellbuf)
    (setq shellbuf (current-buffer)))
  (unless (buffer-live-p shellbuf)
    (error "`%s' buffer does not live." (buffer-name shellbuf)))
  (unless (get-buffer-process shellbuf)
    (error "Process of buffer `%s' does not exist." (buffer-name shellbuf)))
  (with-current-buffer shellbuf
    (unless (equal default-directory dir)
      (goto-char (point-max))
      (comint-kill-input)
      (insert (concat "cd '" dir "'"))
      (comint-send-input))))

;; 引数で指定されたプロセスの名前が shell で子プロセスがない場合は、
;; process-query-on-exit-flag を nil に設定し、
;; "Buffer has a runnig process.; kill it?"
;; のプロンプト表示を抑制する。
(defun shell-plus-unset-query-on-exit-process (proc)
  (when
      (and proc
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
