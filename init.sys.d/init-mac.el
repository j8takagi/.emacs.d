;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X用の設定

;; OSのpath_helperでPATHを取得し、あらためてPATHとして設定
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
       ("EDITOR" "emacsclient")
       ("VISUAL" "emacsclient")
       ))
  (let ((var (car list)) (val (nth 1 list)))
    (setenv var val)))

;; Mac OS X用の文字コード指定
(set-file-name-coding-system 'utf-8-hfs)

(setq locale-coding-system 'utf-8-hfs)

;; フレームの設定
(dolist
    (val
     '(
       (width . 180)
       (height . 55)
       (top . 22)
       (left . 0)
       ))
  (add-to-list 'default-frame-alist val))

(dolist
    (list
     '(
       (jisx0201 "Osaka")
       (japanese-jisx0213.2004-1 "Hiragino Kaku Gothic ProN")
       ))
  (let ((charset (car list))
        (fontfamily (nth 1 list)))
    (cond
     ((not (member charset charset-list))
        (message "Character set %s is not found." charset))
     ((not (member fontfamily (font-family-list)))
        (message "Font family %s is not found." fontfamily))
     ((set-fontset-font t charset (font-spec :family fontfamily))))))

(setq default-input-method "MacOSX")

;; commandキーをEmacsのMetaキーに
(setq mac-command-modifier 'meta)

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

;; emacsclientを使えるように
(eval-after-load "server" '(server-start))

(cd "~")

(provide 'init-mac)
