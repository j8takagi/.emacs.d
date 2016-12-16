;;; init-view-mode.el ---

;; Copyright (C) 2015, 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'view)

;; read-only-mode
(defun turn-on-read-only ()
  "Turn on the current buffer read-only"
  (read-only-mode 1)
  )

(defun turn-off-read-only ()
  "Turn off the current buffer read-only"
  (read-only-mode 0)
  )

(defvar read-write-enable-dir-patterns
  '(
    "~/Documents/201[4-9]_[01][0-9]"
    "~/.emacs.d/elpa"
    "/tmp"
    )
  "Directories pattern to set buffer read-only."
  )

(defun setting-files-read-only ()
  "Set buffer file read only by `read-write-enable-dir-patterns' and
`completion-ignored-extensions'."
  (let ((match nil) (finddir nil) (findext nil))
    (when
        (catch 'finddir
          (dolist (dir read-write-enable-dir-patterns)
            (when (string-match (expand-file-name dir) (buffer-file-name))
              (setq match t)
              (throw 'finddir t))))
      (catch 'findext
        (dolist (ext completion-ignored-extensions)
          (when (string-match (concat ext "\\'") (buffer-file-name))
            (setq match nil)
            (throw 'findext t)))))
    (unless match
      (turn-on-read-only))))

(add-hook 'find-file-hooks 'setting-files-read-only)

(defvar major-mode-disable-view-mode-patterns nil
  ;; '(
  ;;   "dired-.*-?mode"
  ;;   "magit-.*-?mode"
  ;;   "package-.*-?mode"
  ;;   "Info-mode"
  ;;   "\\*Buffer List\\*"
  ;;   "tetris-mode"
  ;;   "Life-mode"
  ;;   "help-mode"
  ;;   "sokoban-mode"
  ;;   )
  "Major mode patterns exclude from setting view-mode if buffer is read-only."
  )

(defun view-mode-if-buffer-read-only ()
  "Turn on view mode if this buffer is read-only."
  (let ((match nil))
    (dolist (mode major-mode-disable-view-mode-patterns)
      (when (string-match mode (symbol-name major-mode))
        (setq match t)))
    (when (and buffer-read-only buffer-file-name match)
      (view-mode nil))))

(add-hook 'after-change-major-mode-hook 'view-mode-if-buffer-read-only)

;; *Messages* バッファーを view-mode に
(save-current-buffer
  (progn
    (set-buffer "*Messages*")
    (view-mode)))

(provide 'init-view-mode)
;;; init-view-mode.el ends here
