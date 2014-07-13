;; -*- mode: Emacs-Lisp; -*-
;; Linuxの設定

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)

(dolist
    (dir (split-string (getenv "PATH") "[:\n]"))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir t)))

;; 環境変数LANGの設定
(setenv "LANG" "en_US.UTF-8")

(provide 'init-linux)
