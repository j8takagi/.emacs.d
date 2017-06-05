;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X GUI用の設定

(message "Start of loading %s." load-file-name)

(require 'my-init)

(my-init-requires
 'fontset-set
 'mac-ime-cursor
 )

;; 環境変数の設定
(my-init-setenv
 '("LANG" "en_US.UTF-8")
 '("PAGER" "cat")
 '("MANPAGER" "cat")
 '("EDITOR" "emacsclient")
 '("VISUAL" "emacsclient")
 )

(my-init-custom-set-variables
 `(default-frame-alist                  ; デフォルトフレーム
    (
     (font
      ,(fontset-set
        '(
          (ascii . (font-spec :family "Menlo" :weight 'normal :slant 'normal :size 12))
          (unicode . (font-spec :family "Hiragino Sans"))
          )
        "mydefault_mac"))
     (width 180)
     (height 56)
     (top 23)
     (left 0)
     ))
 '(mac-command-modifier meta)        ; commandキーをEmacsのMetaキーに
 '(mac-auto-ascii-mode 1)             ; ミニバッファへのカーソル移動時、日本語IMEを自動オフ
 )

;; view-modeの設定
(with-eval-after-load 'view
  (my-init-custom-set-variables
   '(set-view-mode-read-write-directory-patterns
     (
      "~/Documents/201[4-9]_[01][0-9]"
      "~/.emacs.d/elpa" "/tmp" "/var"
      ))))

; IMEのオンとオフにあわせ、カーソルの色を変える
(mac-ime-cursor-add-hook)

;; Mac OS Xのキー設定
(my-init-global-set-keys
 '("<M-f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
 )

(cd "~")

(provide 'init-mac-gui)
