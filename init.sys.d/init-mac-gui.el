;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X 非terminal 用の設定
(require 'my-init)

;; Mac OS Xのpath_helperでPATHを取得し、あらためてPATHとして設定
(let ((shell-file-name "/bin/bash"))
    (setenv "PATH"
            (shell-command-to-string
             "eval $(/usr/libexec/path_helper -s) && printf $PATH")))

;; 環境変数の設定
(dolist
    (envval
     '(
       ("LANG" "en_US.UTF-8")
       ("PAGER" "cat")
       ("MANPAGER" "cat")
       ("EDITOR" "emacsclient")
       ("VISUAL" "emacsclient")
       ))
  (setenv (car envval) (cadr envval)))

;; 標準のフォントサイズとフォントファミリーの設定
(set-face-attribute 'default nil
                    :height 120
                    :family "Menlo")

;; キャラクターセットごとにフォントファミリーを設定
(dolist
    (charfont                           ; キャラクターセットごとのフォントファミリー
     '(
       (jisx0201 "Osaka")
       (japanese-jisx0213.2004-1 "Hiragino Kaku Gothic ProN")
       (japanese-jisx0213-2 "Hiragino Kaku Gothic ProN")
       ))
  (my-init-set-fontfamily (car charfont) (cadr charfont)))

;; commandキーをEmacsのMetaキーに
(setq mac-command-modifier 'meta)

;; ミニバッファにカーソルを移動する際、自動的にキーボードをASCIIモードにする
(mac-auto-ascii-mode 1)

;; Mac OS Xのキー設定
(dolist                                 ; グローバルのキーバインド
    (mapkeys
     '(
       ("<M-f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
      ))
  (my-init-global-set-key (car mapkeys) (cadr mapkeys)))

(cd "~")

(provide 'init-mac-gui)
