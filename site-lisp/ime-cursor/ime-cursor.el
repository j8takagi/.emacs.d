;;; ime-cursor.el --- IMEをオンにしたときのカーソルの色を変える

;; Copyright (C) 2017 by j8takagi

;; Authors: j8takagi
;; Keywords: ime cursor

;;; Commentary:

;;; Code:
(defcustom ime-cursor-color "navy" "Cursor color when IME is on." :group 'display)

(defvar ime-cursor-original-color (face-attribute 'cursor :background))

(defun ime-cursor-set-color ()
  "Set cursor color to `ime-cursor-color'."
  (setq ime-cursor-original-color (face-attribute 'cursor :background))
  (set-cursor-color ime-cursor-color))

(defun ime-cursor-unset-color ()
  "Unset cursor color to default."
  (interactive)
  (set-cursor-color ime-cursor-original-color))

(provide 'ime-cursor)
;;; ime-cursor.el ends here
