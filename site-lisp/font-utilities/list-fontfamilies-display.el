;;; list-fontfamilies-display.el ---

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(defcustom list-fontfamilies-sample-text
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 0123456789 !\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"
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
  (let
      ((all (zerop (length regexp))) (max-length 0) line-format fontfamilies)
    ;; We filter and take the max length in one pass
    (setq fontfamilies
          (delete-dups
           (delq nil (mapcar
                      (lambda (f)
                        (when (or all (string-match-p regexp f))
                          (setq max-length (max (length f) max-length))
                          f))
                      (sort (font-family-list) #'string-lessp)))))
    (unless fontfamilies
      (error "No font families matching \"%s\"" regexp))
    (setq max-length (1+ max-length)
          line-format (format "%%-%ds" max-length))
    (with-help-window "*Font families*"
      (with-current-buffer "*Font families*"
        (setq truncate-lines t)
        (dolist (afontfamily fontfamilies)
          (insert (propertize (format line-format afontfamily) 'face (list :overline t)))
          (let ((apos (point)) (abeg (line-beginning-position)))
            (insert (propertize list-fontfamilies-sample-text 'face (list :family afontfamily)))
            (insert "\n")
            (list-fontfamilies-line-up apos abeg max-length)))
        (goto-char (point-min))))))

(defun list-fontfamilies-line-up (pos line-begin max-length)
  "If the sample text has multiple lines, line up all of them."
  (goto-char pos)
  (forward-line 1)
  (while (not (eobp))
    (insert-char ?\s max-length)
    (forward-line 1)))

(provide 'list-fontfamilies-display)
;;; list-fontfamilies-display.el ends here
