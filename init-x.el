;;;-*-Emacs-Lisp-*-
;;; X-Windowの設定
(provide 'init-x)

;; Emacs Server
(require 'server)
(unless (server-running-p)
    (server-start))

;; Edit with Emacs in Chromium
(require 'edit-server)
(unless (process-status "edit-server")
  (edit-server-start))
(setq edit-server-new-frame nil)

;; フレームの設定
(setq default-frame-alist
      (append (list
               '(foreground-color . "black")
               '(background-color . "gray99")
               '(cursor-color . "DarkOliveGreen")
               '(width . 120)
               '(height . 34)
               '(top . 0)
               '(left . 0)
               '(cursor-type . box))
              default-frame-alist))

;; ツールバーを表示しない
(tool-bar-mode 0)

(global-unset-key "\C-\\")

(set-face-attribute 'default nil
                     :family "IPAGothic"
                     :height 130)
(set-fontset-font "fontset-default"
                     'japanese-jisx0208
                     '("IPAGothic" . "jisx0208-sjis"))
