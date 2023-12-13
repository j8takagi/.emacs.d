;;; view-mode-init.el -*- lexical-binding: t -*-

;; Copyright (C) 2014, 2015, 2017, 2023 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords: file view

;;; Commentary:


;;; Code:
(require 'view)

(defcustom view-mode-init-read-write-directory-patterns
  nil
  "Directory patterns to set buffer read-only."
  :group 'files
  :type '(repeat regexp)
  )

(defcustom view-mode-init-exclude-major-mode-patterns
  nil
  "Major mode patterns exclude from setting view-mode if buffer is read-only."
  :group 'files
  :type '(repeat regexp)
  )

(defun view-mode-init-files-read-only ()
  "Set files to enable read-only-mode when opening the files.
Exceptions are defined as directories in
`view-mode-init-read-write-directory-patterns' and
file patterns in `completion-ignored-extensions'."
  (let (match)
    (when
        (catch 'finddir
          (dolist (dir view-mode-init-read-write-directory-patterns)
            (when (string-match (expand-file-name dir) (buffer-file-name))
              (setq match t)
              (throw 'finddir t))))
      (catch 'findext
        (dolist (ext completion-ignored-extensions)
          (when (string-match (concat ext "\\'") (buffer-file-name))
            (setq match nil)
            (throw 'findext t)))))
    (unless match
      (read-only-mode 1))))

(add-hook 'find-file-hook 'view-mode-init-files-read-only)

(defun view-mode-init-buffers-read-only ()
  "Turn on view mode read-only buffers."
  (let ((match nil))
    (catch 'findmode
      (dolist (aptn view-mode-init-exclude-major-mode-patterns)
        (when (string-match aptn (symbol-name major-mode))
          (setq match t)
          (throw 'findmode t))))
    (when (and buffer-read-only buffer-file-name match)
      (view-mode))))

;(add-hook 'after-change-major-mode-hook 'view-mode-init-buffers-read-only)

(defun view-mode-init-buffers (&rest buffer-name-regexp)
  "Turn on view mode buffers match BUFFER-NAME-REGEXP."
  (dolist (abuf (buffer-list))
    (catch 'findbuf
      (dolist (aregexp buffer-name-regexp)
        (when (string-match-p aregexp (buffer-name abuf))
          (with-current-buffer abuf (view-mode))
          (throw 'findbuf t))))))

(provide 'view-mode-init)
;;; view-mode-init.el ends here
