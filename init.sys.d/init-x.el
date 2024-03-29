;; -*- lexical-binding: t -*-
;;; X-Windowの設定

(message "Start of loading %s." load-file-name)

(require 'listify)

(listify-requires
 'fontset-set
 'view-mode-init
 )

;(exec-path-from-shell-initialize)

;; 標準のフォントサイズとフォントファミリーの設定
(listify-set
 `(default-frame-alist                  ; デフォルトフレーム
    (
     (font
      ,(fontset-set
        '(
          (ascii (font-spec :family "Noto Sans Mono" :weight 'normal :slant 'normal :size 13))
          (unicode (font-spec :family "Noto Sans CJK JP" :weight 'light :size 13))
          )
        "mydefault_x"))
     (width 160)
     (height 45)
     (top 0)
     (left 0)
     )))

;; view-modeの設定
(with-eval-after-load 'view
  (listify-set
   '(view-mode-init-read-write-directory-patterns
     (
      "~" "/tmp" "/var"
      ))))

(provide 'init-x)
