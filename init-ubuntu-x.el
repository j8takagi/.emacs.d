;;;-*-Emacs-Lisp-*-
;;; Ubuntu Linuxの設定
(provide 'init-ubuntu-x)

;; Emacs Server
(server-start)

(defvar ex-open-cmd "xdg-open")

;;; バッファのファイルをオープン
(defun ex-open-file (filename)
  "Open file with external program"
  (let ((process-connection-type nil))
    (start-process "external open" nil ex-open-cmd filename)))

;;; バッファからファイルをオープン
(defun ex-open-file-from-buffer ()
  "Open the buffer file with external program"
  (interactive)
  (if buffer-file-name
      (ex-open-file (buffer-file-name))
    (error "This buffer is not visiting a file")))

(global-set-key "\C-x\M-\C-f" 'ex-open-file-from-buffer)

;; カーソル下のファイルやディレクトリを関連付けられたプログラムで開く
(defun dired-ex-open-file ()
  "Open the current file with external program"
  (interactive)
  (ex-open-file (dired-get-filename)))

;; 現在のディレクトリを関連付けられたプログラムで開く
(defun dired-cd-ex-open ()
  "Open current directory with external program"
  (interactive)
  (ex-open-file (expand-file-name dired-directory)))

;; キーバインド
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "r") 'dired-ex-open-file)
            (define-key dired-mode-map (kbd "C-c .") 'dired-cd-ex-open)))
