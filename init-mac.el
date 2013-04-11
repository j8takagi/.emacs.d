;;;-*-Emacs-Lisp-*-
;;; Mac OS X用の設定
(provide 'init-mac)

;; OSのpath_helperでPATHを取得し、あらためてPATHとして設定
(let ((shell-file-name "/bin/bash"))
    (setenv "PATH" (shell-command-to-string "eval $(/usr/libexec/path_helper -s) && echo -n $PATH")))

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
               '(width . 240)
               '(height . 65)
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

(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))

(setq default-input-method "MacOSX")

(global-unset-key "\C-\\")

;; Info
(setq Info-default-directory-list
      '("/usr/local/share/info/"
        "/usr/local/share/info/ja"
        "/usr/share/info/"))

;; commandキーをEmacsのMetaキーとして設定
(setq mac-command-modifier 'meta)

;; optionキーはEmacsでは使わない
;; (setq mac-option-modifier nil)
;; ほかのアプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
(global-set-key '[s-f1] 'other-frame)
