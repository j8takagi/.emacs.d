;;; quicklook.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: QuickLook

;;; Commentary:

;;; Code:
(require 'dired)

;;; ファイルをQuickLookで開く
(defun quicklook (file)
  "QuickLook a file in MacOS."
  ;;; 指定したファイルが実在しない場合はエラー終了
  (unless (file-exists-p file)
    (error "%s: File does not exist." file))
  (let (cmdstr (procname "quicklook"))
    (setq cmdstr
          (mapconcat
           'identity
           (list "qlmanage" "-p" (shell-quote-argument file))
           " "))
    (message "%s at %s: %s" procname (format-time-string "%Y/%m/%d %H:%M:%S") cmdstr)
    (start-process-shell-command procname (messages-buffer) cmdstr)))

;; dired-modeでカーソル下のファイルやディレクトリーをQuickLookで開く
(defun quicklook-dired ()
  "Open file mentioned on this line in QuickLook."
  (interactive)
  (quicklook (dired-get-filename)))

(provide 'quicklook)
;;; quicklook.el ends here
