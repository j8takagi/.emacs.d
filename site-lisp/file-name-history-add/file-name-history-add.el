;;; file-name-history-add.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(defun file-name-history-add (filename)
  (add-to-history 'file-name-history filename)
  filename)

(advice-add 'dired-get-file-for-visit :filter-return #'file-name-history-add)

(provide 'file-name-history-add)
;;; file-name-history-add.el ends here
