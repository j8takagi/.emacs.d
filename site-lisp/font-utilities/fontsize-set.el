;;; fontsize-set.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(defvar fontsize-set-history nil)

(defun fontsize-set-get-font-pixels ()
  "Get selected frame font pixels which means font size."
  (string-to-number
   (nth 7
        (split-string
         (frame-parameter nil 'font) "-"))))

(defun fontsize-set-update-xlfd-pixels (xlfd size)
  "Update XLFD pixels which means font size and Return updated XLFD."
  (let (xl newxlfd)
    (setq xl (split-string xlfd "-"))
    ;; pixels of font size is xlfd 7th element.
    (setf (nth 7 xl) (number-to-string size))
    (setq newxlfd (mapconcat 'identity xl "-"))
    newxlfd))

(defun fontsize-set (size)
  "Set font size of selected frame."
  (interactive
   (list (read-number
          (format "Font size (now: %d): " (fontsize-set-get-font-pixels))
          nil fontsize-set-history)))
  (let (oldxlfd newxlfd oldsize)
    (setq
     oldxlfd (frame-parameter nil 'font)
     oldsize (fontsize-set-get-font-pixels)
     newxlfd (fontsize-set-update-xlfd-pixels oldxlfd size))
    (set-frame-font newxlfd 1)
    (message "Frame font size `%d' is changed to `%d'" oldsize size)
    ))

(provide 'fontsize-set)
;;; fontsize-set.el ends here
