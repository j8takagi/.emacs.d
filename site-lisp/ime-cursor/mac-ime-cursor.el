;;; mac-ime-cursor-color.el --- Mac Ports Emacsで、IMEのオンとオフにあわせてカーソルの色を変える

;; Copyright (C) 2017 by j8takagi

;; Authors: j9takagi
;; Keywords: ime cursor mac

;;; Commentary:


;;; Code:
(require 'ime-cursor)

(defun mac-ime-cursor-change-color ()
  "IMEのオンとオフにあわせ、カーソルの色を変える"
  (interactive)
  (catch 'match
    (dolist
        (imptn
         '(
           "^com\\.justsystems.inputmethod.atok[0-9]+\\.Japanese"
           "^com\\.apple\\.inputmethod\\.Kotoeri\\.Japanese"
           "^com\\.google\\.inputmethod\\.Japanese"
           ))
      (when (string-match imptn (mac-input-source))
        (ime-cursor-set-color)
        (throw 'match t)))
    (ime-cursor-unset-color)))

(provide 'mac-ime-cursor)
;;; mac-ime-cursor.el ends here
