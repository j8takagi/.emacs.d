;;; list-fontfamilies-display.el ---

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:

(defcustom list-fontfamilies-sample-text
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789 !\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"
  "Text string to display as the sample text for `list-fontfamilies-display'."
  :type 'string
  :group 'display
  )

(defun list-fontfamilies-display (&optional regexp)
  "List all font familiees, using the same sample text in each.
The sample text is a string that comes from the variable
`list-fontfamilies-sample-text'.

If REGEXP is non-nil, list only those font families with names matching
this regular expression.  When called interactively with a prefix
argument, prompt for a regular expression using `read-regexp'."
  (interactive (list (and current-prefix-arg
                          (read-regexp "List font families matching regexp"))))
  (let ((all (zerop (length regexp))) (max-length 0) line-format setfonts)
    ;; We filter and take the max length in one pass
    (setq setfonts
          (delete-dups
           (delq nil (mapcar
                      (lambda (f)
                        (when (or all (string-match-p regexp f))
                          (setq max-length (max (length f) max-length))
                          f))
                      (sort (font-family-list) #'string-lessp)))))
    (unless setfonts
      (error "No font families matching \"%s\"" regexp))
    (setq max-length (1+ max-length)
          line-format (format "%%-%ds" max-length))
    (with-help-window "*Font families*"
      (with-current-buffer "*Font families*"
        (setq truncate-lines t)
        (dolist (afont setfonts)
          (insert (propertize (format line-format afont) 'face (list :overline t)))
          (let ((beg (point)) (line-beg (line-beginning-position)))
            (insert (propertize list-fontfamilies-sample-text 'face (list :family afont)))
            (insert "\n")
            ;; If the sample text has multiple lines, line up all of them.
            (goto-char beg)
            (forward-line 1)
            (while (not (eobp))
              (insert-char ?\s max-length)
              (forward-line 1))))
        (goto-char (point-min))))))

(provide 'list-fontfamilies-display)
;;; list-fontfamilies-display.el ends here
