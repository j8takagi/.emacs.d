;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X GUI用の設定

(message "Start of loading init-mac-gui.")

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

(my-init-set-alist
 `(default-frame-alist                  ; デフォルトフレーム
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

(defun mac-set-ime-cursor-color ()
  "IMEのオンとオフにあわせ、カーソルの色を変える"
  (interactive)
  (catch 'match
    (dolist
        (imptn
         '(
           "^com\\.justsystems.inputmethod.atok[0-9]+\\.Japanese"
           "^com\\.apple\\.inputmethod\\.Kotoeri\\.Japanese"
           "^com\\.google\\.inputmethod\\.Japanese"
           ))
      (when (string-match imptn (mac-input-source))
        (ime-cursor-set-color)
        (throw 'match t)))
    (ime-cursor-unset-color)))

;; カーソル色を、IMの変更時とEmacsの画面を表示したときに設定する
(my-init-set-hooks
 '(mac-selected-keyboard-input-source-change-hook mac-set-ime-cursor-color)
 '(focus-in-hook mac-set-ime-cursor-color)
 )

;; カスタム変数の設定
(my-init-set-variables
 '(mac-command-modifier meta)        ; commandキーをEmacsのMetaキーに
 '(mac-auto-ascii-mode 1)             ; ミニバッファへのカーソル移動時、日本語IMEを自動オフ
 )

;; view-modeの設定
(with-eval-after-load 'view
  (my-init-set-list
   '(read-write-enable-dir-patterns
     (
      "~/Documents/201[4-9]_[01][0-9]"
      "~/.emacs.d/elpa"
      "/tmp"
      "/var"
      ))))

;; Mac OS Xのキー設定
(my-init-global-set-keys
 '("<M-f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
 )

(cd "~")

(provide 'init-mac-gui)
