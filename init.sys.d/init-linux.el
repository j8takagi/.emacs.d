;;;-*-Emacs-Lisp-*-
;;; Linuxの設定

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)
(dolist
    (dir (split-string (getenv "PATH") "[:\n]"))
  (when (and (not (member dir exec-path)) (file-exists-p dir))
    (add-to-list 'exec-path dir t)))

;; 環境変数LANGの設定
(setenv "LANG" "en_US.UTF-8")

;; 文字コードのデフォルトはUTF-8
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-unix)

;; ターミナルの文字コード UTF-8
(set-terminal-coding-system 'utf-8)

(provide 'init-linux)
