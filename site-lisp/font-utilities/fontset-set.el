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
    (set-fontset-font fontset charset fontspec nil 'append)))

(defun fontset-set-fontfamily (fontset charset fontfamily)
  "Use FONTFAMILY for character set CHARSET."
  (if (not (member fontfamily (font-family-list)))
      (message "Warning: In setting font family, font family %s is not available." fontfamily)
    (fontset-set-font-spec fontset charset (font-spec :family fontfamily))))

(defun fontset-set-create-fontset (&optional basename asciifont)
  "create fontset. It returns a name of the created fontset."
  (interactive)
  (unless asciifont
    (setq asciifont (frame-parameter nil 'font)))
  (create-fontset-from-ascii-font asciifont nil basename))

(defun fontset-set-charset-font (fontset charset-font-alist)
  "set font in CHARSET-FONT-ALIST to the FONTSET."
  (interactive)
  (unless fontset
    (error "Argument FONTSET is nil"))
  (unless charset-font-alist
    (error "Argument CHARSET-FONT-ALIST is nil"))
  (let (charsetfonts acharsetfont)
    (setq charsetfonts charset-font-alist)
    (while (progn
             (setq acharsetfont (car charsetfonts))
             (fontset-set-font-spec fontset (car acharsetfont) (eval (cdr acharsetfont)))
             (setq charsetfonts (cdr charsetfonts))))))

(defun fontset-set (charset-font-alist &optional fontset-basename)
  "create fontset using FONTSET-BASENAME, then set font in CHARSET-FONT-ALIST.
if CHARSET-FONT-ALIST is nil, `fontset-set-charset-font-alist' to the fontset.
It returns a name of the created fontset."
  (interactive)
  (let (afontset)
    (setq afontset
          (fontset-set-create-fontset
           fontset-basename
           (font-xlfd-name (eval (cdr (car charset-font-alist))))))
    (fontset-set-charset-font afontset charset-font-alist)
    afontset))

(provide 'fontset-set)
;;; fontset-set.el ends here
