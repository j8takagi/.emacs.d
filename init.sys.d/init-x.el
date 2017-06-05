;; -*- mode: Emacs-Lisp; -*-
;;; X-Windowの設定

(message "Start of loading %s." load-file-name)

(require 'my-init)

(my-init-requires
 'fontset-set
 )

;; 標準のフォントサイズとフォントファミリーの設定
(my-init-custom-set-alist
 `(default-frame-alist                  ; デフォルトフレーム
    (font
     ,(fontset-set
       '(
         (ascii . (font-spec :family "Inconsolata" :weight 'normal :slant 'normal :size 14))
         (unicode . (font-spec :family "TakaoExゴシック"))
         )
       "mydefault_x"))
    (width 180)
    (height 50)
    (top 0)
    (left 0)
    ))

;; view-modeの設定
(with-eval-after-load 'view
  (my-init-custom-set-list
   '(set-view-mode-read-write-directory-patterns
     (
      "/media/.+/Documents/" "~/.emacs.d/elpa" "/tmp" "/var"
      ))))

(provide 'init-x)
