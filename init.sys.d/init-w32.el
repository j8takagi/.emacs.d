;; -*- mode: Emacs-Lisp; -*-
;; MS-Windowsの設定
(require 'my-init)

;;; フォントの設定
(custom-set-variables
 '(fontset-set-charset-font-list
   '(
     (ascii (font-spec :family "Consolas" :weight 'normal :size 12))
     (unicode (font-spec :family "游ゴシック"))
     (katakana-jisx0201 (font-spec :family "ＭＳ ゴシック"))
     )))

(my-init-fontset-set-frame "mydefault")

;; フレームの設定
(dolist
    (fparam                             ; フレームパラメーター
     '(
       (width 180)
       (height 55)
       (top 22)
       (left 0)
       (font "fontset-mydefault")
       ))
  (add-to-list 'default-frame-alist (cons (car fparam) (cadr fparam))))

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8-dos)

;; 日本語ファイル名を正常に処理するための設定
(set-variable 'default-file-name-coding-system 'cp932)
(set-variable 'default-process-coding-system '(utf-8 . cp932))

;; 環境変数EDITORの設定
(setenv "EDITOR" "emacsclient")

;; Shell-modeの文字コード設定
(defun set-buffer-process-coding-system-cp932 ()
  (set-buffer-process-coding-system 'cp932 'cp932))

;; IME切り替え時に undefined のエラーメッセージが表示されるのを抑制
(set-variable 'default-input-method "W32-IME")

(dolist                                 ; グローバルのキーバインド
    (mapkeys
     '(
       ("<M-kanji>" ignore)
       ("<kanji>" toggle-input-method)
       ))
  (my-init-global-set-key (car mapkeys) (cadr mapkeys)))

;; フックの設定
(dolist
    (hookfunc                           ; フックに設定するファンクション
     '(
       (shell-mode-hook set-buffer-process-coding-system-cp932)
       (w32-ime-on-hook ime-cursor-set-color)
       (w32-ime-off-hook ime-cursor-unset-color)
       ))
  (my-init-set-hook (car hookfunc) (cadr hookfunc)))

(cd "~")

(provide 'init-w32)
