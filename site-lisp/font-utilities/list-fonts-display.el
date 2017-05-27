;;; list-fonts-display.el ---

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'fontset-set)
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

(defun list-fonts-display (&optional regexp)
  "List available fonts, using the same sample text in each.
The sample text is a string that comes from the variable
`list-fonts-sample-text'.

If REGEXP is non-nil, list only those font families with names matching
this regular expression.  When called interactively with a prefix
argument, prompt for a regular expression using `read-regexp'."
  (interactive (list (and current-prefix-arg
                          (read-regexp "List font families matching regexp"))))
  (let (saved-fontset aalist)
    (setq saved-fontset (face-attribute 'default :fontset))
    (set-frame-font
     (create-fontset-from-ascii-font
      (frame-parameter (selected-frame) 'font) nil "list_fontfamilies")
     nil nil)
    (unless (setq aalist (list-fonts-alist regexp))
      (error "No font families matching \"%s\"" regexp))
    (list-fonts-alist-display aalist)
    (set-frame-font saved-fontset nil)
    ))

(defun list-fonts-alist (&optional regexp)
  "Return alist (FONTFAMILY-NAME . XLFD) of font families.

If REGEXP is nil, list all font families. If REGEXP is non-nil,
list only those font families with names matching this
regular expression."
  (let (aalist)
    (dolist (afontfamily (delete-dups (sort (font-family-list) #'string-lessp)))
      (when (or (zerop (length regexp)) (string-match-p regexp afontfamily))
        (dolist (axlfd (delete-dups (x-list-fonts afontfamily)))
          (push (cons afontfamily axlfd) aalist))))
    (nreverse aalist)))

(defun list-fonts-alist-display (alist)
  (let
      ((max-length 0) (abuf "*Fonts*")
       afontfamily aface afontprop line-format)
    (dolist (acell alist)
      (setq max-length (max (length (cdr acell)) max-length)))
    (setq max-length (1+ max-length)
          line-format (format "%%-%ds" max-length))
    (select-frame-set-input-focus (make-frame))
    (switch-to-buffer (get-buffer-create abuf))
    (with-help-window abuf
      (setq truncate-lines t)
      (catch 'nextfont)
      (dolist (acell alist)
        (setq afontfamily (car acell) afontprop (cdr acell))
        (insert (propertize (format line-format afontprop) 'face (list :overline t)))
        (setq aface (intern (concat "list-fonts-" afontprop)))
        (condition-case aerr
            (set-face-attribute aface (selected-frame)
                                :width 'normal :weight 'normal
                                :slant 'normal :font afontprop)
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
