;;;-*-Emacs-Lisp-*-
;;; Ubuntu Linuxの設定
(provide 'init-ubuntu-x)

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)
(dolist
    (adir (split-string (getenv "PATH") "[:\n]"))
  (when (and (not (member adir exec-path)) (file-exists-p adir))
    (add-to-list 'exec-path adir t)))

;; 環境変数LANGの設定
(setenv "LANG" "en_US.UTF-8")

;; Emacs Server
(require 'server)
(unless (server-running-p)
    (server-start))

;; フレームの設定
(setq default-frame-alist
      (append (list
               '(foreground-color . "black")
               '(background-color . "gray99")
               '(cursor-color . "DarkOliveGreen")
               '(width . 100)
               '(height . 34)
               '(top . 0)
               '(left . 0)
               '(cursor-type . box))
              default-frame-alist))

;; ツールバーを表示しない
(tool-bar-mode 0)

;; 文字コードのデフォルトはUTF-8
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-unix)

;; ターミナルの文字コード UTF-8
(set-terminal-coding-system 'utf-8)

(global-unset-key "\C-\\")

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
