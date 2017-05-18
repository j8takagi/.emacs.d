;;; char-font.el --- 

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun char-font-xlfd-at-position (position)
  "Return X logical font description (XLFD) of the font at POSITION in the current buffer."
  (if (not (display-graphic-p))
      (message "Display is not graphical. So font is not used.")
    (let (fontobj)
      (setq fontobj (format "%s" (car (internal-char-font position))))
      (string-match "#<font-object \\(.+\\)>" fontobj)
      (match-string 1 fontobj))))

(defun char-fontfamily-at-position (position)
  "Return fontfamily name of the font at POSITION in the current buffer."
  (char-font-xlfd2family (char-font-xlfd-at-position position)))

(defun char-font-xlfd2family (xfld)
  "Return font family name from X logical font description (XLFD)."
  (if (not (string-match "-[^-]+-\\([^-]+?\\)-.+" xfld))
      (error "Argument is not XFLD.")
    (match-string 1 xfld)))

(defun char-font-xlfd-at-point ()
  "Return X logical font description (XLFD) of the font at the point."
  (interactive)
  (message (char-font-xlfd-at-position (point))))

(provide 'char-font)
;;; char-font.el ends here
