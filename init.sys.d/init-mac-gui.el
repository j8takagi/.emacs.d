;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X 非terminal 用の設定

;; Mac OS Xのpath_helperでPATHを取得し、あらためてPATHとして設定
(let ((shell-file-name "/bin/bash"))
    (setenv "PATH"
            (shell-command-to-string
             "eval $(/usr/libexec/path_helper -s) && printf $PATH")))

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)

(dolist
    (dir (split-string (getenv "PATH") "[:\n]"))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir t)))

;; 環境変数の設定
(dolist
    (list
     '(
       ("LANG" "en_US.UTF-8")
       ("PAGER" "cat")
       ("MANPAGER" "cat")
       ("EDITOR" "emacsclient")
       ("VISUAL" "emacsclient")
       ))
  (let ((var (car list)) (val (nth 1 list)))
    (setenv var val)))

;; 標準のフォントサイズとフォントファミリーの設定
(set-face-attribute 'default nil
                    :height 120
                    :family "Menlo")

;; キャラクターセットごとにフォントファミリーを設定
(dolist
    (list
     '(
       (jisx0201 "Osaka")
       (japanese-jisx0213.2004-1 "Hiragino Kaku Gothic ProN")
       (japanese-jisx0213-2 "Hiragino Kaku Gothic ProN")
       ))
  (let ((charset (car list))
        (fontfamily (nth 1 list)))
    (cond
     ((not (member charset charset-list))
        (message "Character set %s is not found." charset))
     ((not (member fontfamily (font-family-list)))
        (message "Font family %s is not found." fontfamily))
     ((set-fontset-font t charset (font-spec :family fontfamily))))))

;; フレームの設定
(dolist
    (val
     '(
       (foreground-color . "black")
       (background-color . "gray99")
       (cursor-color . "DarkOliveGreen")
       (cursor-type . box)
       ))
  (add-to-list 'default-frame-alist val))

;; Mac OS XのIME設定
;(setq default-input-method "MacOSX")

;; commandキーをEmacsのMetaキーに
(setq mac-command-modifier 'meta)

;; ミニバッファにカーソルを移動する際、自動的にキーボードをASCIIモードにする
(mac-auto-ascii-mode 1)

;; Mac OS Xのキー設定
(dolist
    (map
     '(
       ("<M-f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
       ))
  (let ((key (car map)) (func (nth 1 map)))
    (if (not (functionp func))
        (message "%s is not defined." func)
      (global-set-key (kbd key) func))))

(cd "~")

(provide 'init-mac-gui)
