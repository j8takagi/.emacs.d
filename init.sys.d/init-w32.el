;; -*- mode: Emacs-Lisp; -*-
;; MS-Windowsの設定

(message "Start of loading %s." load-file-name)

(require 'listify)

(listify-requires 'fontset-set)

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8-dos)

;; 環境変数EDITORの設定
(listify-setenv
 '("EDITOR" "emacsclient")
 )

(listify-set
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
    )))

(listify-set
 '(default-file-name-coding-system cp932) ; 日本語ファイル名を正常に処理
 '(default-process-coding-system (utf-8 . cp932)) ; 日本語ファイル名を正常に処理
 '(default-input-method "W32-IME")              ; IMEの設定
 )

;; Shell-modeの文字コード設定
(defun set-buffer-process-coding-system-cp932 ()
  (set-buffer-process-coding-system 'cp932 'cp932))

;; view-modeの設定
(with-eval-after-load 'view
  (listify-set
   '(set-view-mode-read-write-directory-patterns
     ("E:/Documents/201[4-9]_[01][0-9]" "~/.emacs.d/elpa"
      ))))

(listify-global-set-keys
 '("<M-kanji>" ignore)             ; IME切り替え時に undefined のエラーメッセージが表示されるのを抑制
 '("<kanji>" toggle-input-method)
 )

;; フックの設定
(listify-set
 '(shell-mode-hook (set-buffer-process-coding-system-cp932))
 '(w32-ime-on-hook (ime-cursor-set-color))
 '(w32-ime-off-hook (ime-cursor-unset-color))
 )

(cd "~")

(provide 'init-w32)
