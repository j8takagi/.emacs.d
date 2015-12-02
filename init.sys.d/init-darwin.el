;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X用の設定

;; Mac OS X用の文字コード指定
(set-file-name-coding-system 'utf-8-hfs)

(setq locale-coding-system 'utf-8-hfs)

(set-keyboard-coding-system nil)

;; Mac OS XのIME設定
(setq default-input-method "MacOSX")

;; commandキーをEmacsのMetaキーに
(setq mac-command-modifier 'meta)

;; Mac OS Xのキー設定
(dolist
    (map
     '(
       ("C-x RET u" ucs-normalize-NFC-buffer)
       ("<M-f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
       ))
  (let ((key (car map)) (func (nth 1 map)))
    (if (not (functionp func))
        (message "%s is not defined." func)
      (global-set-key (kbd key) func))))

;; ミニバッファにカーソルを移動する際、自動的にキーボードをASCIIモードにする
(mac-auto-ascii-mode 1)

(cd "~")

(provide 'init-darwin)
