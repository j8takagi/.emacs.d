;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X 非terminal 用の設定
(require 'my-init)

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
       (japanese-jisx0213.2004-1 "YuGothic")
       (japanese-jisx0213-2 "YuGothic")
       ))
  (my-init-set-fontfamily (car charfont) (cadr charfont)))


;; カスタム変数の設定
(custom-set-variables
 '(mac-command-modifier 'meta)        ; commandキーをEmacsのMetaキーに
 '(mac-auto-ascii-mode 1)             ; ミニバッファへのカーソル移動時、日本語IMEを自動オフ
 )

;; Mac OS Xのキー設定
(dolist                                 ; グローバルのキーバインド
    (mapkeys
     '(
       ("<M-f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
      ))
  (my-init-global-set-key (car mapkeys) (cadr mapkeys)))

(cd "~")

(provide 'init-mac-gui)
