;; -*- mode: Emacs-Lisp; -*-
;; MS-Windowsの設定

(require 'my-init)

;; 標準のフォントサイズとフォントファミリーの設定
(set-face-attribute 'default nil
                    :height 120
                    :family "Consolas")

;; 日本語フォントにメイリオ、半角カナのフォントにＭＳ ゴシックを設定
(dolist
    (charfont                           ; キャラクターセットごとのフォント属性の設定
     '(
       (jisx0201 (font-spec :family "ＭＳ ゴシック"))
       (japanese-jisx0213.2004-1 (font-spec :family "游ゴシック" :weight 'semi-bold))
       (japanese-jisx0213-2 (font-spec :family "游ゴシック" :weight 'semi-bold))
       ))
  (my-init-set-font-spec (car charfont) (eval (cadr charfont))))

;; フレームの設定
(dolist
    (fparam                             ; フレームパラメーター
     '(
       (width 120)
       (height 34)
       (top 0)
       (left 0)
       ))
  (add-to-list 'default-frame-alist (cons (car fparam) (cadr fparam))))

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8-dos)

;; 日本語ファイル名を正常に処理するための設定
(setq default-file-name-coding-system 'cp932)
(setq default-process-coding-system '(utf-8 . cp932))

;; 環境変数EDITORの設定
(setenv "EDITOR" "emacsclient")

;; Shell-modeの文字コード設定
(defun set-buffer-process-coding-system-cp932 ()
  (set-buffer-process-coding-system 'cp932 'cp932))

(add-hook 'shell-mode-hook 'set-buffer-process-coding-system-cp932)

;; IME切り替え時に undefined のエラーメッセージが表示されるのを抑制
(global-set-key (kbd "<M-kanji>") 'ignore)

(cd "~")

(provide 'init-w32)
