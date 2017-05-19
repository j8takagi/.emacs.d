;;; fontset-set.el --- 

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:

(defun fontset-set-font-spec (fontset charset fontspec)
  "Use FONTSPEC for character set CHARSET."
  (if (not (member charset charset-list))
      (message "Warning: character set %s is not defined." charset)
    (set-fontset-font fontset charset fontspec nil 'prepend)))

(defun fontset-set-fontfamily (fontset charset fontfamily)
  "Use FONTFAMILY for character set CHARSET."
  (if (not (member fontfamily (font-family-list)))
      (message "Warning: In setting font family, font family %s is not available." fontfamily)
    (fontset-set-font-spec fontset charset (font-spec :family fontfamily))))

(defcustom fontset-set-charset-font-alist nil
  "List of (CHARSET FONT) to set fontset."
  :type 'alist
  :group 'display
  )

(defun fontset-set (fontset-basename)
  (interactive)
  (fontset-set-charset-font
   (fontset-set-create-fontset fontset-basename)
   fontset-set-charset-font-alist)
  (concat "fontset-" fontset-basename))

(defun fontset-set-create-fontset (fontset-basename &optional asciifont)
  (interactive)
  (let (afontset asciifont)
    (unless asciifont
      (setq asciifont (frame-parameter nil 'font)))
    (create-fontset-from-ascii-font asciifont nil fontset-basename)))

(defun fontset-set-charset-font (fontset charset-font-alist)
  (interactive)
  (let (charsetfonts charsetfont)
    (setq charsetfonts charset-font-alist)
    (while charsetfonts
      (setq charsetfont (car charsetfonts))
      (fontset-set-fontfamily fontset (car charsetfont) (cadr charsetfont))
      (setq charsetfonts (cdr charsetfonts)))))

(defun fontset-set (&optional fontset-basename)
  "create fontset, then set font in `fontset-set-charset-font-alist' to the fontset.
It returns a name of the created fontset."
  (interactive)
  (let (afontset)
    (setq afontset
          (fontset-set-create-fontset
           fontset-basename
           (cadr (assoc 'ascii fontset-set-charset-font-alist))))
    (fontset-set-charset-font afontset
                              fontset-set-charset-font-alist)
    afontset))

(provide 'fontset-set)
;;; fontset-set.el ends here
