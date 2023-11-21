;;; fontset-set.el -*- lexical-binding: t -*-

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'fontset)

(defvar fontset-history nil
  "History list for fontset.")

(defvar fontfamily-history nil
  "History list for font family.")

(defcustom default-fontset "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default"
  "Default fontset."
  :type 'string
  :group 'display)

(defun set-face-fontset (face fontset &optional frame)
  (unless frame
    (setq frame (selected-frame)))
  (set-face-attribute face frame :font fontset :fontset fontset))

(defun buffer-fontset-set (fontset)
  "Set buffer font and fontset to FONTSET."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (completing-read
       "Fontset name: " (fontset-list) nil t nil 'fontset-history))))
  (set-face-fontset (make-local-variable 'buffer-fontset-face) fontset)
  (buffer-face-set 'buffer-fontset-face))

(defun frame-fontset-set (fontset)
  "Set frame font to FONTSET."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (completing-read
       "Fontset name: " (fontset-list) nil t nil 'fontset-history))))
  (set-face-fontset 'default fontset))

(defun fontset-set-font-spec (fontset charset fontspec)
  "Use FONTSPEC for character set CHARSET."
  (unless (member charset charset-list)
      (warn "Character set `%s' is not defined." charset))
  (set-fontset-font fontset charset fontspec nil 'append))

(defun fontset-set-fontfamily (fontset charset fontfamily)
  "Use FONTFAMILY for character set CHARSET."
  (if (not (member fontfamily (font-family-list)))
      (warn "Font family `%s' is not available." fontfamily)
    (fontset-set-font-spec fontset charset (font-spec :family fontfamily))))

(defun fontset-set-alias2spec (alias)
  "Get fontset-spec from ALIAS of fontset."
  (query-fontset (concat "fontset-" alias))
  )

(defun fontset-set-create-fontset (basename)
  "Create fontset from BASENAME. It returns the created fontset name."
  (interactive)
  (create-fontset-from-fontset-spec (concat "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-" basename)))

(defun fontset-set-charset-font (fontset charset-font-alist)
  "Set font in CHARSET-FONT-ALIST to the FONTSET."
  (interactive)
  (let ((res nil))
    (if (null fontset)
        (warn "Argument FONTSET is nil")
      (if (null charset-font-alist)
          (warn "Argument CHARSET-FONT-ALIST is nil")
        (dolist (acharfont charset-font-alist)
          (fontset-set-font-spec fontset (car acharfont) (eval (cadr acharfont))))
        (setq res fontset)))
    res))

(defun fontset-set (charset-font-alist basename)
  "Create fontset using FONTSET-BASENAME, then set font in CHARSET-FONT-ALIST.
if CHARSET-FONT-ALIST is nil, `fontset-set-charset-font-alist' to the fontset.
It returns a name of the created fontset."
  (fontset-set-charset-font
   (fontset-set-create-fontset basename)
   charset-font-alist))

(defun fontsets-set (&rest fontset-spec)
  "Create fontset using FONTSET-SPEC.
Each FONTSET-SPEC has the form:

(CHARSET-FONT-ALIST FONTSET-BASENAME).

CHARSET-FONT-ALIST is association list of:

(TARGET . FONTSPEC).
FONTSET-BASENAME is a string.

If CHARSET-FONT-ALIST is nil,
`fontset-set-charset-font-alist' is set to the fontset.

It returns a name of the created fontset."
  (let (fontsets)
    (dolist (fs fontset-spec)
      (setq fontsets
            (append fontsets
                    (list (fontset-set (car fs) (cadr fs))))))
    fontsets))

(provide 'fontset-set)
;;; fontset-set.el ends here
