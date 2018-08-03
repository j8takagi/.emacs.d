;; -*- mode: Emacs-Lisp; -*-
;;; X-Windowの設定

(message "Start of loading %s." load-file-name)

(require 'listify)

(listify-requires
 'fontset-set
 )

;; 標準のフォントサイズとフォントファミリーの設定
(listify-set
 `(default-frame-alist                  ; デフォルトフレーム
    (
     (font
      ,(fontset-set
        '(
          (ascii . (font-spec :family "Ubuntu Mono" :weight 'normal :slant 'normal :size 16))
          (unicode . (font-spec :family "Noto Sans CJK JP"))
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
   '(set-view-mode-read-write-directory-patterns
     (
      "~" "/tmp" "/var"
      ))))

(provide 'init-x)
