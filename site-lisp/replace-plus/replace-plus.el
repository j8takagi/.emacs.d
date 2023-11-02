;;; replace-plus.el --- 

;; Copyright (C) 2018 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'dired)

;;;###autoload
(defun replace-plus-do-regexp (regexp replacements &optional start end)
  (let ((replace-count 0))
    (save-excursion
      (save-restriction
        (when (and start end)
          (narrow-to-region start end))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match
           (if (stringp replacements)
               replacements
             (funcall (car replacements) (cdr replacements) replace-count))
           t)
          (setq replace-count (+ 1 replace-count)))))
    replace-count))

;;;###autoload
(defun replace-plus-do-string (from-string to-string &optional start end)
  (let ((replace-count 0))
    (save-excursion
      (save-restriction
        (when (and start end)
          (narrow-to-region start end))
        (goto-char (point-min))
        (while (search-forward from-string nil t)
          (replace-match to-string)
          (setq replace-count (+ 1 replace-count)))))
    replace-count))

;;;###autoload
(defun replace-plus-regexp (regexp to-string &optional start end)
  (interactive
   (append
    (replace-plus-read-args "Replace regexp" t)
    (when (use-region-p)
      (list (region-beginning) (region-end)))))
  (message "%s%s: %d regexp replaced. `%s' -> `%s'"
           (buffer-name)
           (if (and start end)
               " (narrowed)"
             "")
           (replace-plus-do-regexp regexp to-string start end)
           regexp
           to-string
           ))

;;;###autoload
(defun replace-plus-string (from-string to-string &optional start end)
  (interactive
   (append
    (replace-plus-read-args "Replace string" nil)
    (when (use-region-p)
      (list (region-beginning) (region-end)))))
   (replace-plus-do-string from-string to-string start end))

;;;###autoload
(defun replace-plus-read-args (prompt regexp-flag)
  (let (from to)
    (setq
     from (query-replace-read-from prompt regexp-flag)
     to
     (if (consp from)
         (prog1 (cdr from) (setq from (car from)))
       (query-replace-read-to from prompt regexp-flag)))
    (list from to)))

;;;###autoload
(defun replace-plus-do-regexp-file (regexp to-string file)
  (let (cnt)
   (with-current-buffer (find-file-noselect file)
     (backup-buffer)
     (setq cnt (replace-plus-do-regexp regexp to-string))
     (basic-save-buffer))
   cnt))

;;;###autoload
(defun replace-plus-do-string-file (from-string to-string file)
  (let (cnt)
   (with-current-buffer (find-file-noselect file)
     (backup-buffer)
     (setq cnt (replace-plus-do-string from-string to-string))
     (basic-save-buffer))
   cnt))

;;;###autoload
(defun replace-plus-dired-regexp-files (regexp to-string)
  (interactive
   (query-replace-read-args "Replace regexp in marked files" t))
  (let (msg (win (selected-window)))
    (setq msg
          (format "replacing regexp in files: `%s' -> `%s'.\n----------------------------------------\n"  regexp to-string))
    (dolist (afile (dired-map-over-marks (dired-get-filename) nil))
      (setq msg (concat
                 msg
                 (format "%s: %d matches replaced.\n"
                         afile
                         (replace-plus-do-regexp-file regexp to-string afile)))))
    (pop-to-buffer (get-buffer-create "*Dired replace-regexp files*"))
    (let ((inhibit-read-only t) (buffer-undo-list t))
      (erase-buffer)
      (insert msg)
      (set-buffer-modified-p nil)
      (select-window win))))

;;;###autoload
(defun replace-plus-dired-string-files (from-string to-string)
  (interactive
   (replace-plus-read-args "Replace string in marked files" nil))
  (let (msg (win (selected-window)))
    (setq msg
          (format "replacing string in files: `%s' -> `%s'.\n----------------------------------------\n"  from-string to-string))
    (dolist (afile (dired-map-over-marks (dired-get-filename) nil))
      (setq msg (concat
                 msg
                 (format "%s: %d matches replaced.\n"
                         afile
                         (replace-plus-do-string-file from-string to-string afile)))))
    (pop-to-buffer (get-buffer-create "*Dired replace-string files*"))
    (let ((inhibit-read-only t) (buffer-undo-list t))
      (erase-buffer)
      (insert msg)
      (set-buffer-modified-p nil)
      (select-window win))))

;;;###autoload
(defun replace-plus-regexp-last-history ()
  (interactive)
  (let ((ato (car query-replace-history)) (afrom (cadr query-replace-history)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward afrom nil t)
        (replace-match ato)))))

;;;###autoload
(defun replace-plus-string-last-history ()
  (interactive)
  (let ((ato (car query-replace-history)) (afrom (cadr query-replace-history)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward afrom nil t)
        (replace-match ato nil 1)))))

(provide 'replace-plus)
;;; replace-plus.el ends here
