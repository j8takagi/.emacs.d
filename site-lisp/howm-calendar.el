(require 'calendar)
(require 'howm-mode)
(setq
 calendar-date-display-form
 '("[" year "-" (format "%02d" (string-to-int month))
   "-" (format "%02d" (string-to-int day)) "]"))
(setq diary-file
      (expand-file-name "diary" howm-directory))

(defun howm-mark-calendar-date ()
  (interactive)
  (require 'howm-reminder)
  (let* ((today (howm-reminder-today 0))
         (limit (howm-reminder-today 1))
         (howm-schedule-types
          howm-schedule-menu-types)
         (raw (howm-reminder-search
               howm-schedule-types))
         (str nil) (yy nil) (mm nil) (dd nil))
    (while raw
      (setq str (nth 1 (car raw)))
      (when
          (string-match
           "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)"
           str)
        (setq yy (match-string 1 str))
        (setq mm (match-string 2 str))
        (setq dd (match-string 3 str)))
      (when (and yy mm dd)
        (mark-calendar-date-pattern
         (string-to-int mm)
         (string-to-int dd)
         (string-to-int yy)))
      (setq mm nil)
      (setq dd nil)
      (setq yy nil)
      (setq raw (cdr raw))
      )))

(defadvice mark-diary-entries
  (after mark-howm-entry activate)
  (howm-mark-calendar-date))

(setq
 howm-menu-display-rules
 (cons
  (cons "%hdiary[\n]?" 'howm-menu-diary)
  howm-menu-display-rules
   ))

(defun howm-menu-diary ()
  (require 'diary-lib)
  (message "scanning diary...")
  (delete-region
   (match-beginning 0) (match-end 0))
  (let* ((now (decode-time (current-time)))
         (diary-date
          (list (nth 4 now) (nth 3 now) (nth 5 now)))
         (diary-display-hook 'ignore)
         (cbuf (current-buffer))
         (howm-diary-entry nil)
         (howm-diary-entry-day nil)
         (str nil))
    (unwind-protect
        (setq howm-diary-entry
              (list-diary-entries
               diary-date howm-menu-schedule-days))
      (save-excursion
        (set-buffer
         (find-buffer-visiting diary-file))
        (subst-char-in-region
         (point-min) (point-max) ?\^M ?\n t)
        (setq selective-display nil)))

    (while howm-diary-entry
      (setq howm-diary-entry-day (car howm-diary-entry))
      (setq mm (nth 0 (car howm-diary-entry-day)))
      (setq dd (nth 1 (car howm-diary-entry-day)))
      (setq yy (nth 2 (car howm-diary-entry-day)))
      (setq str (nth 1 howm-diary-entry-day))
      (setq howm-diary-entry (cdr howm-diary-entry))
      (insert
       (format
        ">>d [%04d-%02d-%02d] %s\n" yy mm dd str))))
  (message "scanning diary...done")
  )

(setq diary-date-forms
      '((month "/" day "[^/0-9]")
        (month "/" day "/" year "[^0-9]")
        ("\\[" year "-" month "-" day "\\]" "[^0-9]")
        (monthname " *" day "[^,0-9]")
        (monthname " *" day ", *" year "[^0-9]")
        (dayname "\\W")))

(defun howm-open-diary (&optional dummy)
  (interactive)
  (let ((date-str nil) (str nil))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward
             ">>d \\(\\[[-0-9]+\\]\\) " nil t)
        (setq str
              (concat
               "^.+"
               (buffer-substring-no-properties
                (point) (line-end-position))))
        (setq date-str
              (concat
               "^.+"
               (buffer-substring-no-properties
                (match-beginning 1)
                (match-end 1))
               " " str))
        (find-file
         (substitute-in-file-name diary-file))
        (howm-mode t)
        (goto-char (point-min))
        (if (re-search-forward date-str nil t)
            ()
          (re-search-forward str nil t))))))

(defun add-diary-action-lock-rule ()
  (let ((rule
         (action-lock-general
          'howm-open-diary
          "^\\(>>d\\) "
          1 1)))
    (if (not (member rule action-lock-default-rules))
        (progn
          (setq action-lock-default-rules
                (cons rule action-lock-default-rules))
          (action-lock-set-rules
           action-lock-default-rules)))))

(add-hook 'action-lock-mode-on-hook
          'add-diary-action-lock-rule)

(defadvice make-diary-entry
  (after howm-mode activate)
  (text-mode)
  (howm-mode t))
