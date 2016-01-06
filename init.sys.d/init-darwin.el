;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X用の設定
(require 'ucs-normalize)

;; Mac OS X用の文字コード指定
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

(provide 'init-darwin)
