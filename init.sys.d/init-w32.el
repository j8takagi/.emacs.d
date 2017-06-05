;; -*- mode: Emacs-Lisp; -*-
;; MS-Windowsの設定

(message "Start of loading %s." load-file-name)

(require 'my-init)

(my-init-requires 'fontset-set)

;; 環境変数EDITORの設定
(my-init-setenv
 '("EDITOR" "emacsclient")
 )


(my-init-custom-set-variables
 `(default-frame-alist                  ; デフォルトフレーム
    (
    (font
     ,(fontset-set
       '(
         (ascii . (font-spec :family "Consolas" :weight 'normal :size 14))
         (unicode . (font-spec :family "メイリオ"))
         )
       "mydefault_w32"))
    (width 160)
    (height 48)
    (top 0)
    (left 0)
    ))
 '(default-file-name-coding-system cp932) ; 日本語ファイル名を正常に処理する
 '(default-input-method "W32-IME")                ; IMEの設定
 '(default-process-coding-system (utf-8 cp932)) ; 日本語ファイル名を正常に処理する
 )

(custom-set-variables
 )

;; view-modeの設定
(with-eval-after-load 'view
  (my-init-custom-set-variables
   '(set-view-mode-read-write-directory-patterns
     ("E:/Documents/201[4-9]_[01][0-9]" "~/.emacs.d/elpa"
      ))))

(my-init-global-set-keys
 '("<M-kanji>" ignore)             ; IME切り替え時に undefined のエラーメッセージが表示されるのを抑制
 '("<kanji>" toggle-input-method)
 )

;; Shell-modeの文字コード設定
(defun set-buffer-process-coding-system-cp932 ()
  (set-buffer-process-coding-system 'cp932 'cp932))

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8-dos)

;; フックの設定
(my-init-custom-set-list
 '(shell-mode-hook (set-buffer-process-coding-system-cp932))
 '(w32-ime-on-hook (ime-cursor-set-color))
 '(w32-ime-off-hook (ime-cursor-unset-color))
 )

(cd "~")

(provide 'init-w32)
