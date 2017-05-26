;;; list-fontfamilies-display.el ---

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'fontset-set)

(defcustom list-fontfamilies-sample-text
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789 !\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~\nあいうえお　アイウエオ　安以宇衣於\n逢芦飴溢茨鰯淫迂厩噂餌襖迦牙廻恢晦蟹葛鞄釜翰翫徽"
  "Text string to display as the sample text for `list-fontfamilies-display'."
  :type 'string
  :group 'display
  )

(defun list-fontfamilies-display (&optional regexp)
  "List all font families, using the same sample text in each.
The sample text is a string that comes from the variable
`list-fontfamilies-sample-text'.

If REGEXP is non-nil, list only those font families with names matching
this regular expression.  When called interactively with a prefix
argument, prompt for a regular expression using `read-regexp'."
  (interactive (list (and current-prefix-arg
                          (read-regexp "List font families matching regexp"))))
  (let ((max-length 0) aalist afontfamily aface afontprop avector line-format)
    ;; We filter and take the max length in one pass
    (unless (setq aalist (list-fontfamilies-alist regexp))
      (error "No font families matching \"%s\"" regexp))
    (dolist (alst aalist)
      (setq max-length (max (length (cdr alst)) max-length)))
    (setq max-length (1+ max-length)
          line-format (format "%%-%ds" max-length))
    (with-help-window "*Font families*"
      (with-current-buffer "*Font families*"
        (setq truncate-lines t)
        (dolist (alst aalist)
          (setq afontfamily (car alst) afontprop (cdr alst))
          (internal-make-lisp-face
           (setq aface (intern (concat "list-fontfamilies-" afontprop))) (selected-frame))
          (if (string-match-p "-" afontfamily)
              (set-face-attribute aface (selected-frame) :family afontfamily :weight 'normal :slant 'normal)
            (set-face-font aface afontprop (selected-frame)))
          (insert (propertize (format line-format afontprop) 'face (list :overline t)))
          (let ((beg (point)) (line-beg (line-beginning-position)))
            (insert (propertize list-fontfamilies-sample-text
                                'face aface))
            (insert "\n")
            ;; If the sample text has multiple lines, line up all of them.
            (goto-char beg)
            (forward-line 1)
            (while (not (eobp))
              (insert-char ?\s (- max-length 8))
              (forward-line 1))))
      (goto-char (point-min))))))

(defun list-fontfamilies-alist (&optional regexp)
  "Alist (FONTFAMILY-NAME . XLFD) of font families.

If REGEXP is nil, list all font families. If REGEXP is non-nil,
list only those font families with names matching this
regular expression.  When called interactively with a prefix
argument, prompt for a regular expression using `read-regexp'."
  (let (aalist)
    (dolist (afontfamily (delete-dups (sort (font-family-list) #'string-lessp)))
      (when (or (zerop (length regexp)) (string-match-p regexp afontfamily))
        (dolist (axlfd (delete-dups (x-list-fonts afontfamily)))
          (push (cons afontfamily axlfd) aalist))))
    (nreverse aalist)))

(provide 'list-fontfamilies-display)
;;; list-fontfamilies-display.el ends here
