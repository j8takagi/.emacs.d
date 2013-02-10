;;;-*-Emacs-Lisp-*-
;;; Linuxの設定
(provide 'init-linux)

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)
(dolist
    (adir (split-string (getenv "PATH") "[:\n]"))
  (when (and (not (member adir exec-path)) (file-exists-p adir))
    (add-to-list 'exec-path adir t)))

;; 環境変数LANGの設定
(setenv "LANG" "en_US.UTF-8")

(setenv "INFOPATH" "/usr/local/share/info:/usr/share/info:/usr/local/share/emacs/site-lisp/magit/share/info:~/share/info")

;; 文字コードのデフォルトはUTF-8
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-unix)

;; ターミナルの文字コード UTF-8
(set-terminal-coding-system 'utf-8)

(global-unset-key "\C-\\")
