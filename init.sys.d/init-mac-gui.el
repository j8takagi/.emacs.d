;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X 非terminal 用の設定
(require 'my-init)
(my-init-require 'fontset-set)

;; 環境変数の設定
(dolist
    (envval                             ; 環境変数ごとの設定値
     '(
       ("LANG" "en_US.UTF-8")
       ("PAGER" "cat")
       ("MANPAGER" "cat")
       ("EDITOR" "emacsclient")
       ("VISUAL" "emacsclient")
       ))
  (setenv (car envval) (cadr envval)))

;;; フォントの設定
(custom-set-variables
 '(fontset-set-charset-font-alist
   '(
     (unicode "YuGothic")
     (ascii "Menlo")
     (katakana-jisx0201 "Osaka")
     )))

(fontset-set "mydefault")
(add-to-list 'default-frame-alist '(font . "fontset-mydefault"))
;; フレームの設定
(when (equal window-system 'mac)
  (dolist
      (fparam ;; フレームパラメーター
       '(
         (width 180)
         (height 55)
         (top 22)
         (left 0)
         ))
    (add-to-list 'default-frame-alist (cons (car fparam) (cadr fparam)))))

(defun mac-set-ime-cursor-color ()
  "IMEのオン／オフで、カーソルの色を変える"
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

(add-hook 'mac-selected-keyboard-input-source-change-hook
            'mac-set-ime-cursor-color)

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
