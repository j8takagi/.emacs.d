;; -*- mode: Emacs-Lisp; -*-
;; MS-Windowsの設定

(message "Start of loading init-w32.")

(require 'my-init)

;; フレームの設定
(my-init-set-default-frame-alist
 `(
   (font
    ,(fontset-set
      '(
        ;; (unicode . (font-spec :family "源ノ角ゴシック Code JP R" :weight 'normal :size 12))
        (ascii . (font-spec :family "Consolas" :weight 'normal :size 12))
        (unicode . (font-spec :family "游ゴシック"))
        )
      "mydefault_w32"))
   (width 180)
   (height 56)
   (top 0)
   (left 0)
   ))

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8-dos)

(my-init-set-variables
 '(
   (default-file-name-coding-system cp932) ; 日本語ファイル名を正常に処理する
   (default-process-coding-system (utf-8 . cp932)) ; 日本語ファイル名を正常に処理する
   (default-input-method "W32-IME")                ; IMEの設定
   ))

;; 環境変数EDITORの設定
(my-init-setenv
 '(
   ("EDITOR" "emacsclient")
   ))

;; view-modeの設定
(with-eval-after-load 'view
  (custom-set-variables
   '(read-write-enable-dir-patterns
     '(
       "E:/Documents/201[4-9]_[01][0-9]"
       "~/.emacs.d/elpa"
     ))))

(my-init-global-set-keys
     '(
       ("<M-kanji>" ignore)             ; IME切り替え時に undefined のエラーメッセージが表示されるのを抑制
       ("<kanji>" toggle-input-method)
       ))

;; Shell-modeの文字コード設定
(defun set-buffer-process-coding-system-cp932 ()
  (set-buffer-process-coding-system 'cp932 'cp932))

;; フックの設定
(my-init-set-hooks
 '(
   (shell-mode-hook set-buffer-process-coding-system-cp932)
   (w32-ime-on-hook ime-cursor-set-color)
   (w32-ime-off-hook ime-cursor-unset-color)
   ))

(cd "~")

(provide 'init-w32)
