;;; ime-cursor.el --- IMEをオンにしたときのカーソルの色を変える

;; Copyright (C) 2017 by j8takagi

;; Authors: j8takagi
;; Keywords: ime cursor

;;; Commentary:

;;; Code:
(defcustom ime-cursor-color "navy" "Cursor color when IME is on." :group 'display)

(defun ime-cursor-set-color ()
  (interactive)
  "Set cursor color to `ime-cursor-color'."
  (set-cursor-color ime-cursor-color))

(defun ime-cursor-unset-color ()
  "Unset cursor color to default frame parameter, or black."
  (interactive)
    (set-cursor-color
     (or (assoc-default 'cursor-color default-frame-alist) "black")))

(provide 'ime-cursor)
;;; ime-cursor.el ends here
