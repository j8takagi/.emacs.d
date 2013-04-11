;;;-*-Emacs-Lisp-*-
;;; MS-Windowsの設定
(provide 'init-w32)

(cd "~")

;; Emacs Server
(require 'server)
(unless (server-running-p)
    (server-start))

;; フレームの設定
(setq default-frame-alist
      (append (list
               '(foreground-color . "black")
               '(background-color . "gray99")
               '(cursor-color . "DarkOliveGreen")
               '(width . 150)
               '(height . 36)
               '(top . 0)
               '(left . 0)
               '(cursor-type . box))
              default-frame-alist))

;; ツールバーを表示しない
(tool-bar-mode 0)

(set-face-attribute 'default nil
                     :family "Meiryo UI"
                     :height 100)

;; 文字コードのデフォルトはUTF-8
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-unix)
