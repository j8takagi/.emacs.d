;;;-*-Emacs-Lisp-*-
;;; Mac OS X用の設定

;; OSのpath_helperでPATHを取得し、あらためてPATHとして設定
(let ((shell-file-name "/bin/bash"))
    (setenv "PATH" (shell-command-to-string "eval $(/usr/libexec/path_helper -s) && printf $PATH")))

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)

(dolist
    (dir (split-string (getenv "PATH") "[:\n]"))
  (when (and (not (member dir exec-path)) (file-exists-p dir))
    (add-to-list 'exec-path dir t)))

;; 環境変数LANGの設定
(setenv "LANG" "en_US.UTF-8")

;; 環境変数EDITORの設定
(setenv "EDITOR" "emacsclient")

;; Emacs Server
(require 'server)

(unless (server-running-p)
    (server-start))

;; フレームの設定
(dolist
    (val
     '(
       (width . 180)
       (height . 55)
       (top . 22)
       (left . 0)
       ))
  (add-to-list 'default-frame-alist val))

;; ツールバーを表示しない
(tool-bar-mode 0)

;; 文字コードのデフォルトはUTF-8
(set-default-coding-systems 'utf-8)

(require 'ucs-normalize)

(defun ucs-normalize-NFC-buffer ()
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max)))

(setq file-name-coding-system 'utf-8-hfs)

(setq locale-coding-system 'utf-8-hfs)

;; ターミナルの文字コード UTF-8
(set-terminal-coding-system 'utf-8)

(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))

(setq default-input-method "MacOSX")

;; commandキーをEmacsのMetaキーに
(setq mac-command-modifier 'meta)


;; Mac OS Xのアプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
(global-set-key [M-f1] 'other-frame)

;; 起動時のカレントディレクトリが"/"になってしまう件への対応
(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf)
     (set-buffer buf)
     (cd (expand-file-name "~")))
   (buffer-list)))

(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

(global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)  ; バッファ全体の濁点分離を直す

(provide 'init-mac)
