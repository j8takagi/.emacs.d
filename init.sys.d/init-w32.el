;; -*- mode: Emacs-Lisp; -*-
;; MS-Windowsの設定
(require 'my-init)

;; フレームの設定
(let (afontset)
  (setq afontset
        (fontset-set
         '(
           ;; (unicode . (font-spec :family "源ノ角ゴシック Code JP R" :weight 'normal :size 12))
           (ascii . (font-spec :family "Consolas" :weight 'normal :size 12))
           (unicode . (font-spec :family "游ゴシック"))
           (katakana-jisx0201 . (font-spec :family "ＭＳ ゴシック"))
           )
         "mydefault"))
  (dolist
      (fparam                           ; フレームパラメーター
       `(
         (font ,afontset)
         (width 180)
         (height 56)
         (top 22)
         (left 0)
         ))
    (update-or-add-alist default-frame-alist (car fparam) (cadr fparam)))
    (message "default-frame-alist set in init-ma-gui.el - %s" default-frame-alist))

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8-dos)

;; 日本語ファイル名を正常に処理するための設定
(dolist
    (coding                             ; フレームパラメーター
     '(
       (default-file-name-coding-system cp932)
       (default-process-coding-system (utf-8 . cp932))
       ))
     (set-variable (car coding) (cadr coding)))

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
