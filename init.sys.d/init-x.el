;;;-*-Emacs-Lisp-*-
;;; X-Windowの設定

;; Emacs Server
(require 'server)
(unless (server-running-p)
    (server-start))

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

;; ツールバーを表示しない
(tool-bar-mode 0)

(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "IPAGothic"))

(provide 'init-x)
