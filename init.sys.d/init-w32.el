;; -*- mode: Emacs-Lisp; -*-
;;; MS-Windowsの設定

;; フレームの設定
(dolist
    (val
     '(
       (width . 120)
       (height . 34)
       (top . 22)
       (left . 0)
       )))

;; 英語フォントの設定
(set-face-attribute 'default nil
                     :family "Consolas"
                     :height 120)

;; 日本語フォントの設定
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "Meiryo"))

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8-dos)

;; 日本語ファイル名を正常に処理するための設定
(setq default-file-name-coding-system 'shift_jis)

;; emacsclientを使えるように
(eval-after-load "server" '(server-start))

;; 環境変数EDITORの設定
(setenv "EDITOR" "emacsclient")

(cd "~")

(provide 'init-w32)
