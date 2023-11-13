;;; quicklookup.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: Quick Lookup

;;; Commentary:

;;; Code:
(require 'dired)

;;; ファイルをQuick Lookupで開く
(defun quicklookup (file)
  "Quick look a file in Mac OS."
  ;;; 指定したファイルが実在しない場合はエラー終了
  (unless (file-exists-p file)
    (error "%s: File does not exist." file))
  (let (cmdstr (procname "quicklookup"))
    (setq cmdstr
          (mapconcat 'identity
                     (list "qlmanage" "-p" (shell-quote-argument file))
                     " "))
    (message "%s at %s: %s" procname (format-time-string "%Y/%m/%d %H:%M:%S") cmdstr)
    (start-process-shell-command procname (messages-buffer) cmdstr)))

;; dired-modeでカーソル下のファイルやディレクトリーをQuick Lookupで開く
(defun quicklookup-dired ()
  "Open file mentioned on this line in Quick Lookup."
  (interactive)
  (quicklookup (dired-get-filename)))

(provide 'quicklookup)
;;; quicklookup.el ends here
