;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X用の設定

;; Mac OS X用の文字コード指定
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; Mac OS Xのキー設定
(dolist
    (map
     '(
       ("C-x RET u" ucs-normalize-NFC-buffer)
       ))
  (let ((key (car map)) (func (nth 1 map)))
    (if (not (functionp func))
        (message "%s is not defined." func)
      (global-set-key (kbd key) func))))

(provide 'init-darwin)
