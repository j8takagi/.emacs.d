;; -*- mode: Emacs-Lisp; -*-
;; MS-Windowsの設定

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

;; 標準のフォントサイズとフォントファミリーの設定
(set-face-attribute 'default nil
                    :height 120
                    :family "Consolas")

;; 日本語フォントの設定
(dolist
    (list
     '(
       (jisx0201 "ＭＳ ゴシック")
       (japanese-jisx0213.2004-1 "メイリオ")
       (japanese-jisx0213-2 "メイリオ")
       ))
  (let ((charset (car list))
        (fontfamily (nth 1 list)))
    (cond
     ((not (member charset charset-list))
        (message "Character set '%s' is not found." charset))
     ((not (member fontfamily (font-family-list)))
        (message "Font family '%s' is not found." fontfamily))
     ((set-fontset-font t charset (font-spec :family fontfamily))))))

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8-dos)

;; 日本語ファイル名を正常に処理するための設定
(setq default-file-name-coding-system 'shift_jis)

;; emacsclientを使えるように
(eval-after-load "server"
  '(unless (server-running-p) (server-start)))

;; 環境変数EDITORの設定
(setenv "EDITOR" "emacsclient")

(cd "~")

(provide 'init-w32)
