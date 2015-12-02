;; -*- mode: Emacs-Lisp; -*-
;; tiger用の設定

;; フレームの設定
(dolist
    (val
     '(
       (width . 180)
       (height . 55)
       (top . 22)
       (left . 0)
       ))
  (add-to-list 'default-frame-alist val))

(provide 'init-tiger)
;;; init-tiger.el ends here
