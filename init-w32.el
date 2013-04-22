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
               '(width . 120)
               '(height . 34)
               '(top . 0)
               '(left . 0)
               '(cursor-type . box))
              default-frame-alist))

;; ツールバーを表示しない
(tool-bar-mode 0)

(set-face-attribute 'default nil
                     :family "Consolas"
                     :height 120)

;; 日本語フォントの設定
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "Meiryo"))

;; 文字コードのデフォルトはUTF-8
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-unix)

;; 日本語ファイル名を正常に処理するための設定
(setq default-file-name-coding-system 'shift_jis)
