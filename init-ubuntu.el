;;;-*-Emacs-Lisp-*-
;;; Ubuntu Linuxの設定
(provide 'init-ubuntu)

(global-unset-key "\C-\\")

(let ((shell-file-name "/bin/bash"))
    (setenv "PATH" (shell-command-to-string ". ~/.bashrc && echo -n $PATH")))

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)
(dolist
    (adir (split-string (getenv "PATH") "[:\n]"))
  (when (and (not (member adir exec-path)) (file-exists-p adir))
    (add-to-list 'exec-path adir t)))

;; 環境変数LANGの設定
;(setenv "LANG" "en_US.UTF-8")
