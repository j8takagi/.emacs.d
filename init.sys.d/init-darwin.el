;; -*- mode: Emacs-Lisp; -*-
(require 'listify)

(message "Start of loading %s." load-file-name)

;; Mac OS X用の設定
(listify-requires 'ucs-normalize)

;; Mac OS X用の文字コード指定
(listify-set
 '(file-name-coding-system utf-8-hfs)
 '(locale-coding-system utf-8-hfs)
 )

(provide 'init-darwin)
