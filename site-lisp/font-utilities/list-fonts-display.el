;;; list-fonts-display.el ---

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:

;;; Code:
(require 'list-fontfamilies-display)

(defcustom list-fonts-sample-text
  "\nABCDEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789 !\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~\nあいうえお　アイウエオ　安以宇衣於\n逢芦飴溢茨鰯淫迂厩噂餌襖迦牙廻恢晦蟹葛鞄釜翰翫徽"
  "Text string to display as the sample text for `list-fonts-display'."
  :type 'string
  :group 'display
  )

(defcustom list-fonts-sample-text
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 0123456789 !\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"
  "Text string to display as the sample text for `list-fonts-display'."
  :type 'string
  :group 'display
  )

(defun list-fonts-display (&optional regexp &rest fontspecs)
  "List available fonts, using the same sample text in each.
The sample text is a string that comes from the variable
`list-fonts-sample-text'.

If REGEXP is non-nil, list only those font families with names matching
this regular expression.  When called interactively with a prefix
argument, prompt for a regular expression using `read-regexp'.


If FONTSPECS is not nil, FONTSPECS elements are same as ARGS of font-spec.
See `font-spec' and `set-face-attribute'.
FONTSPECS elements must come in pairs KEY VALUE of font properties.
KEY must be a valid font property name listed below:

‘:family’, ‘:weight’, ‘:slant’, ‘:width’,
`:foundry', `:adstyle', `:registry', `:size', `:name',
`:script', `:lang', `:otf'"
  (interactive
   (list
    (and current-prefix-arg
         (read-regexp "List font families matching regexp"))))
  (let (saved-fontset aalist)
    (setq saved-fontset (face-attribute 'default :fontset))
    (set-frame-font
     (create-fontset-from-ascii-font
      (frame-parameter (selected-frame) 'font) nil "list_fontfamilies")
     nil nil)
    (unless (setq aalist (list-fonts-alist regexp fontspecs))
      (error "No font families matching \"%s\"" regexp))
    (list-fonts-list-display (mapcar 'cdr aalist))
    (set-frame-font saved-fontset nil)
    ))

(defun list-fonts-alist (&optional regexp fontspec-list)
  "Return list of alist (FONTFAMILY-NAME . XLFD).

If REGEXP is nil, list all font families. If REGEXP is non-nil,
list only those font families with names matching this
regular expression.

If FONTSPEC-LIST is not nil, FONTSPEC-LIST elements are
same as ARGS of font-spec. See `font-spec' and `set-face-attribute'.
FONTSPEC-LIST elements must come in pairs KEY VALUE of font properties.
KEY must be a valid font property name listed below:

`:family’, `:weight’, `:slant’, `:width’,
`:foundry', `:adstyle', `:registry', `:size', `:name',
`:script', `:lang', `:otf'"
  (let (aalist afonts)
    (dolist (afontfamily (delete-dups (sort (font-family-list) #'string-lessp)))
      (when (or (zerop (length regexp)) (string-match-p regexp afontfamily))
        (condition-case aerr
            (setq afonts
                  (x-list-fonts
                   (font-xlfd-name
                    (apply 'font-spec :family afontfamily fontspec-list))))
          (error
            (setq afonts (x-list-fonts afontfamily))))
        (dolist (axlfd (delete-dups afonts))
          (push (cons afontfamily axlfd) aalist))))
    (nreverse aalist)))

(defun list-fonts-list-display (fontslist)
  "List fonts in FONTLIST, using the same sample text in each.
The sample text is a string that comes from the variable.
`list-fonts-sample-text'."
  (let
      ((max-length 0) (abuf "*Fonts*")
       afontfamily aface line-format)
    (dolist (afont fontslist)
      (setq max-length (max (length afont) max-length)))
    (setq max-length (1+ max-length)
          line-format (format "%%-%ds" max-length))
    (select-frame-set-input-focus (make-frame))
    (switch-to-buffer (get-buffer-create abuf))
    (with-help-window abuf
      (setq truncate-lines t)
      (dolist (afont fontslist)
        (insert (propertize (format line-format afont) 'face (list :overline t)))
        (setq aface (make-local-variable (intern (concat "list-fonts-" afont "-face"))))
        (condition-case aerr
            (set-face-attribute aface (selected-frame) :font afont)
          (error
           (set-face-attribute aface (selected-frame)
                               :foreground
                               (face-attribute 'default ':background))))
        (let ((apos (point)) (abeg (line-beginning-position)))
          (insert (propertize list-fonts-sample-text 'face aface) "\n")
          (list-fontfamilies-line-up apos abeg max-length)))
      (goto-char (point-min)))))

(provide 'list-fonts-display)
;;; list-fonts-display.el ends here
