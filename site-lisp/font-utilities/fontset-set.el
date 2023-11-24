;;; fontset-set.el -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'fontset)

(defvar fontset-set-history nil
  "History list for fontset.")

(defvar fontfamily-history nil
  "History list for font family.")

(defcustom default-fontset "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default"
  "Default fontset."
  :type 'string
  :group 'display)

(defun fontset-set-face (face fontset &optional frame)
  (unless frame
    (setq frame (selected-frame)))
  (set-face-attribute face frame :font fontset :fontset fontset))

(defun fontset-set-buffer (fontset)
  "Set buffer font and fontset to FONTSET."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (completing-read
       "Fontset name: " (fontset-list) nil t nil 'fontset-set-history))))
  (fontset-set-face (make-local-variable 'buffer-fontset-face) fontset)
  (buffer-face-set 'buffer-fontset-face))

(defun fontset-set-frame (fontset)
  "Set frame font to FONTSET."
  (interactive
    (let ((completion-ignore-case t))
      (list
       (completing-read
        "Fontset name: " (fontset-list) nil t nil 'fontset-set-history))))
  (let ((wp (frame-pixel-width)) (hp (frame-pixel-height)))
    (fontset-set-face 'default fontset)
    (set-frame-size nil wp hp 1)))

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

(TARGET . FONTSPEC)

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
