;;; xlfd-at.el --- 

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun xlfd-at-position (position)
  "Return X logical font description (XLFD) of the font at POSITION in the current buffer."
  (if (not (display-graphic-p))
      (message "Display is not graphic. So font is not used.")
    (let (fontobj)
      (setq fontobj (format "%s" (font-at position)))
      (string-match "#<font-object \\(.+\\)>" fontobj)
      (match-string 1 fontobj))))

(defun xlfd-at-xlfd2family (xlfd)
  "Return font family name from X logical font description (XLFD)."
  (if (not (string-match "-[^-]+-\\([^-]+?\\)-.+" xlfd))
      (error "Argument is not xlfd.")
    (match-string 1 xlfd)))

(defun xlfd-at-fontfamily-position (position)
  "Return fontfamily name of the font at POSITION in the current buffer."
  (xlfd-at-xlfd2family (xlfd-at-position position)))

(defun xlfd-at-fontfamily ()
  "Return font-family of the font at the point."
  (interactive)
  (message (xlfd-at-fontfamily-position (point))))

(defun xlfd-at ()
  "Return X logical font description (XLFD) of the font at the point."
  (interactive)
  (message (xlfd-at-position (point))))

(provide 'xlfd-at)
;;; xlfd-at.el ends here
