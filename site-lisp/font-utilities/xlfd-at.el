;;; xlfd-at.el -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'fontset)

(defun xlfd-at (pos)
  "Return X logical font description (XLFD) of
the font at POS in the current buffer."
  (if (not (display-graphic-p))
      (message "Display is not graphic. So font is not used.")
    (font-xlfd-name (font-at pos))))

(defun xlfd2fontfamily (xlfd)
  "Return font family name from X logical font description (XLFD)."
  (aref (x-decompose-font-name xlfd) xlfd-regexp-family-subnum))

(defun fontfamily-cursor-position (pos)
  "Return font-family of the font at the point."
  (interactive "d")
  (message (xlfd2fontfamily (xlfd-at pos))))

(defun xlfd-cursor-position (pos)
  "Return X logical font description (XLFD) of the font at the point."
  (interactive "d")
  (message (xlfd-at pos)))

(provide 'xlfd-at)
;;; xlfd-at.el ends here
