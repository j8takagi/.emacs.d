;; -*- mode: Emacs-Lisp; -*-
;;; X-Windowの設定

;; フレームの設定
(dolist
    (val
     '(
       (width . 150)
       (height . 40)
       (top . 0)
       (left . 0)
       ))
  (add-to-list 'default-frame-alist val))

;; IPAゴシックフォントを使う
(dolist
    (list
     '(
       (japanese-jisx0213.2004-1 "IPAexゴシック")
       (japanese-jisx0213-2 "IPAexゴシック")
       ))
  (let ((charset (car list))
        (fontfamily (nth 1 list)))
    (cond
     ((not (member charset charset-list))
        (message "Character set %s is not found." charset))
     ((not (member fontfamily (font-family-list)))
        (message "Font family %s is not found." fontfamily))
     ((set-fontset-font t charset (font-spec :family fontfamily))))))


(provide 'init-x)
