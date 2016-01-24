;; -*- mode: Emacs-Lisp; -*-
;; tiger用の設定

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

(provide 'init-tiger)
;;; init-tiger.el ends here
