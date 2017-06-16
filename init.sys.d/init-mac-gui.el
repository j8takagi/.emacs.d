;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X GUI用の設定

(message "Start of loading %s." load-file-name)

(require 'listify)

(listify-requires
 'fontset-set
 'mac-ime-cursor
 )

;; 環境変数の設定
(listify-setenv
 '("LANG" "en_US.UTF-8")
 '("PAGER" "cat")
 '("MANPAGER" "cat")
 '("EDITOR" "emacsclient")
 '("VISUAL" "emacsclient")
 )

(listify-set
 `(default-frame-alist                  ; デフォルトフレーム
    (
     (font
      ,(fontsets-set
        '((
          (ascii . (font-spec :family "Menlo" :weight 'normal :slant 'normal :size 12))
          (unicode . (font-spec :family "Hiragino Sans"))
          ) "mydefault_mac")))
     (width 180)
     (height 56)
     (top 23)
     (left 0)
     ))
; '(face-font-rescale-alist (("Hiragino Sans" 1.167) ("YuGothic" 1.167)))
 '(mac-command-modifier meta)        ; commandキーをEmacsのMetaキーに
 '(mac-auto-ascii-mode t) ; ミニバッファへのカーソル移動時、日本語IMEを自動オフ
 )


(fontsets-set
 '((
    (unicode . (font-spec :family "Hiragino Mincho ProN" :weight 'normal :slant 'normal :size 14))
    ) "my_mincho")
 '((
    (unicode . (font-spec :family "YuKyokasho" :weight 'normal :slant 'normal :size 14))
    ) "my_yukyokasho")
 '((
    (unicode . (font-spec :family "YuMincho" :weight 'normal :slant 'normal :size 14))
    ) "my_yumincho")
 '((
    (ascii . (font-spec :family "Menlo" :weight 'normal :slant 'normal))
    (unicode . (font-spec :family "YuGothic" :weight 'normal :slant 'normal))
    ) "my_yugothic")
 '((
    (unicode . (font-spec :family "Hiragino Maru Gothic ProN" :weight 'normal :slant 'normal :size 14))
    ) "my_marugothic")
 '((
    (unicode . (font-spec :family "Klee" :weight 'normal :slant 'normal :size 14))
    ) "my_klee")
 '((
    (unicode . (font-spec :family "Source Han Code JP" :weight 'normal :slant 'normal :size 12))
    ) "my_sourcehancode")
 )

;; view-modeの設定
(with-eval-after-load 'set-view-mode
  (listify-set
   '(set-view-mode-read-write-directory-patterns
     (
      "~/Documents/201[4-9]_[01][0-9]"
      "~/.emacs.d/elpa" "/tmp" "/var"
      ))))

;; Mac OS Xのキー設定
(listify-global-set-keys
 '("<M-f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
 )

(listify-set
 '(mac-selected-keyboard-input-source-change-hook mac-ime-cursor-change-color)
 '(focus-in-hook mac-ime-cursor-change-color)
 )

(cd "~")

(message "%s" (fontset-list))
(provide 'init-mac-gui)
