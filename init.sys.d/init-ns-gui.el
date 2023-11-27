;; Mac OS X GUI用の設定 -*- lexical-binding: t -*-

(message "Start of loading %s." load-file-name)

(require 'listify)

(listify-requires
 'dictionary_app
 'quicklook
 )

;; Mac OS Xのpath_helperでPATHを取得し、あらためてPATHとして設定
(let ((shell-file-name "/bin/bash"))
    (setenv "PATH"
            (shell-command-to-string
             "eval $(/usr/libexec/path_helper -s) && printf $PATH")))

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)

(dolist
    (dir (split-string (getenv "PATH") "[:\n]"))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir t)))

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
     (width 180)
     (height 56)
     (top ,(frame-control-workarea-top))
     (left ,(frame-control-workarea-left))
     (font
      ,(fontset-set
        '(
          (ascii (font-spec :family "Menlo" :weight 'normal :slant 'normal :size 12))
          (unicode (font-spec :family "Hiragino Sans"))
          )
        "my_default_mac"))
     ))
 '(ns-command-modifier meta)        ; commandキーをEmacsのMetaキーに
 '(ns-alternate-modifier none)
 )

(fontset-sets
 '((
    (ascii (font-spec :family "Menlo" :weight 'bold :slant 'normal :size 12))
    (unicode (font-spec :family "Hiragino Sans" :weight 'bold :slant 'normal))
    )
   "my_bold")
 '((
    (ascii (font-spec :weight 'normal :slant 'italic))
    (unicode (font-spec :family "Hiragino Sans" :weight 'semibold :slant 'normal))
    )
   "my_italic")
 '((
    (ascii (font-spec :family "Menlo" :weight 'bold :slant 'italic :size 12))
    (unicode (font-spec :family "Hiragino Sans" :weight 'bold :slant 'normal))
    )
   "my_bold_italic")
 '((
    (unicode (font-spec :family "Hiragino Sans" :weight 'normal :slant 'normal :size 12))
    )
   "my_gothic")
 '((
    (unicode (font-spec :family "Hiragino Mincho ProN" :weight 'normal :slant 'normal :size 12))
    )
   "my_mincho")
 '((
    (unicode (font-spec :family "YuKyokasho" :weight 'normal :slant 'normal :size 12))
    ) 
   "my_yu_kyokasho")
 '((
    (unicode (font-spec :family "YuMincho" :weight 'normal :slant 'normal :size 12))
    ) 
   "my_yu_mincho")
 '((
    (ascii (font-spec :family "Menlo" :weight 'normal))
    (unicode (font-spec :family "YuGothic"))
    ) 
   "my_yu_gothic")
 '((
    (unicode (font-spec :family "Toppan Bunkyu Gothic"))
    ) 
   "my_bunkyu_gothic")
 '((
    (unicode (font-spec :family "Hiragino Maru Gothic ProN"))
    ) 
   "my_maru_gothic")
 '((
    (unicode (font-spec :family "Klee"))
    ) 
   "my_klee")
 '((
    (unicode (font-spec :family "Source Han Code JP"))
    ) 
   "my_sourcehancode")
 '((
   (ascii (font-spec :family "Source Han Code JP" :weight 'normal :slant 'normal :size 12))
   (unicode (font-spec :family "Toppan Bunkyu Gothic" :weight 'normal :slant 'normal))
    ) 
   "my_alternative")
 )

(fontset-set-face 'italic (fontset-set-alias2spec "my_italic"))

;; view-modeの設定
(with-eval-after-load 'set-view-mode
  (listify-set
   '(set-view-mode-read-write-directory-patterns
     (
      "~/Documents"
      "~/.emacs.d/elpa" "/tmp" "/var"
      ))))

;; Mac OS Xのキー設定
(listify-global-set-keys
 '("M-<f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
 '("C-c C-M-d" dictionary_app)
 '("C-c C-d" dictionary_app-at-point)
 )

(listify-modemap-set-keys
 '(dired-mode-map "dired" nil
   (
    ("SPC" quicklook-dired))))

(remove-hook 'first-change-hook 'ns-unselect-line)

(provide 'init-ns-gui)
