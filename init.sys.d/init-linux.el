;; -*- mode: Emacs-Lisp; -*-
;; Linuxの設定

(message "Start of loading %s." load-file-name)

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
