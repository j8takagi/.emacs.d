;; init-w32.el -*- lexical-binding: t -*-
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
         (ascii (font-spec :family "Consolas" :weight 'normal :size 25))
         (unicode (font-spec :family "メイリオ"))
         )
       "mydefault_w32"))
    (width 180)
    (height 56)
    (top 0)
    (left 0)
    )))

(fontsets-set
 '((
    (unicode (font-spec :family "游明朝" :weight 'normal :slant 'normal :size 25))
    )
   "my_mincho")
 '((
    (unicode (font-spec :family "游ゴシック" :weight 'normal :slant 'normal))
    )
   "my_yugothic")
 '((
    (unicode (font-spec :family "源ノ角ゴシック Code JP R" :weight 'normal :slant 'normal :size 25))
    )
   "my_sourcehancode")
 )

(listify-set
 '(default-file-name-coding-system cp932) ; 日本語ファイル名を正常に処理
 '(default-process-coding-system (utf-8 . cp932)) ; プロセスで日本語を正常に処理
 '(default-input-method "W32-IME")              ; IMEの設定
 )

;; Shell-modeの文字コード設定
(defun set-buffer-process-coding-system-cp932 ()
  (set-process-coding-system 'cp932 'cp932))

;; view-modeの設定
(with-eval-after-load 'view
  (listify-set
   '(set-view-mode-read-write-directory-patterns
     ("/Documents/"
      ))))

(listify-global-set-keys
 '("M-<kanji>" ignore)             ; IME切り替え時に undefined のエラーメッセージが表示されるのを抑制
 '("<kanji>" toggle-input-method)
 )

;; フックの設定
(listify-set-hooks
 '(shell-mode-hook (set-buffer-process-coding-system-cp932))
 )

(provide 'init-w32)
