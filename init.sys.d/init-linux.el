;; -*- mode: Emacs-Lisp; -*-
;; Linuxの設定

(message "Start of loading %s." load-file-name)

;; ;; Emacs変数exec-pathに、環境変数PATHの内容を設定
;; (setq exec-path nil)

;; (dolist
;;     (dir (split-string (getenv "PATH") "[:\n]"))
;;   (when (file-directory-p dir)
;;     (add-to-list 'exec-path dir t)))

;; 環境変数LANGの設定
(listify-setenv
 '("LANG" "en_US.UTF-8")
 '("LANGUAGE" "en")
 '("PAGER" "cat")
 '("MANPAGER" "cat")
 '("EDITOR" "emacsclient")
 '("VISUAL" "emacsclient")
 )

(provide 'init-linux)
