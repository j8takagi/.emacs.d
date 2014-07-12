;; -*- mode: Emacs-Lisp; -*-
;;; X-Windowの設定

;; フレームの設定
(dolist
    (val
     '(
       (width . 120)
       (height . 34)
       (top . 0)
       (left . 0)
       ))
  (add-to-list 'default-frame-alist val))

;; IPAゴシックフォントを使う
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "IPAGothic"))

;; emacsclientを使えるように
(eval-after-load "session" (server-start))

(provide 'init-x)
