;; -*- mode: Emacs-Lisp; -*-
;;; X-Windowの設定

(message "Start of loading %s." load-file-name)

;; 標準のフォントサイズとフォントファミリーの設定
(set-face-attribute 'default nil
                    :height 120)

;; 日本語フォントにIPAゴシックフォントを設定
(dolist
    (charfont                           ; キャラクタセットごとのフォントファミリー
     '(
       (japanese-jisx0213.2004-1 "IPAexゴシック")
       (japanese-jisx0213-2 "IPAexゴシック")
       ))
  (my-init-set-fontfamily (car charfont) (cadr charfont)))

;; フレームの設定
(dolist
    (fparam                             ; フレームパラメーター
     '(
       (width 150)
       (height 40)
       (top 0)
       (left 0)
       ))
  (add-to-list 'default-frame-alist (cons (car fparam) (cadr fparam))))

(provide 'init-x)
