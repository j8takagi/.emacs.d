;;; ert-mode.el ---

;; Copyright (C) 2016 by Kazuhito Takagi

;; Authors: Kazuhito Takagi
;; Keywords: ert erd database

;;; Commentary:


;;; Code:

(defgroup ert-mode nil
  "Transcription using mpv."
  :prefix "ert-"
  :group 'wp)

(defcustom ert-mode-hook nil
  "Normal hook when entering `ert-mode'."
  :type 'hook
  :group 'ert-mode)

(defvar ert-mode-map nil)

;;; Font lock
(require 'font-lock)

(defgroup ert-faces nil
  "Faces used in ERT Mode"
  :group 'ert-mode
  :group 'faces)

(defface ert-comment-face
  '((t (:bold nil :foreground "dark red")))
  "face for comments in ERT mode."
  :group 'ert-mode)

(defface ert-entity-face
  '((t (:bold t :foreground "dark green")))
  "face for comments in ERT mode."
  :group 'ert-mode)

(defface ert-attribute-face
  '((t (:bold nil :foreground "black")))
  "face for comments in ERT mode."
  :group 'ert-mode)

(defface ert-pk-face
  '((t (:bold t :foreground "dim gray")))
  "face for comments in ERT mode."
  :group 'ert-mode)

(defface ert-relation-face
  '((t (:bold nil :foreground "dark green")))
  "face for comments in ERT mode."
  :group 'ert-mode)

(defface ert-colopt-face
  '((t (:bold nil :foreground "dark blue")))
  "face for comments in ERT mode."
  :group 'ert-mode)

(defun ert-mode ()
  "This mode is for editing ert files."
  (interactive)
  (setq major-mode 'ert-mode
        mode-name "ert mode")
  (setq ert-mode-map (make-keymap))
  ;; (dolist
  ;;     (map
  ;;      '(
  ;;        ))
  ;;   (let ((key (car map)) (func (nth 1 map)))
  ;;     (if (not (functionp func))
  ;;         (message "Warning: function `%s' is NOT defined." func)
  ;;       (define-key ert-mode-map (kbd key) func))))
  (use-local-map ert-mode-map)
  (setq indent-line-function 'indent-relative)
  (setq font-lock-defaults
        '((
          ("#.*$" . 'ert-comment-face)
          ("^[^ ]+$" . 'ert-entity-face)
          ("^[ \t]+[^ \\[+]+" . 'ert-attribute-face)
          ("^[ \t]+\\+[^ \\[]+" . 'ert-pk-face)
          (">.+" . 'ert-relation-face)
          ("\\[.+?\\]" . 'ert-colopt-face)
          ))))

(provide 'ert-mode)
;;; ert-mode.el ends here
