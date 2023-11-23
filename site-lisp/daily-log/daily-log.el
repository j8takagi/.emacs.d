;;; daily-log.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(defgroup daily-log nil
  "daily-log-mnode for editing daily log."
  :prefix "daily-log-"
  :group 'text)


(defcustom daily-log-directory "~/Documents/daily_log/"
  "Daily log directory"
  :type '(directory)
)

(defcustom daily-log-interval 1
  "Daily log timer interval"
 :type '(number)
)

(defun daily-log-file (time)
  "Get daily-log file name of the time."
  (expand-file-name
   (concat daily-log-directory (format-time-string "%Y-%m-%d" time) ".txt")))

(defvar daily-log-timer nil
  "Daily log timer interval"
)

(defun daily-log-save ()
  "Save daily-log file."
  (interactive)
  (let ((save-silently 1)
        (dbuf (get-file-buffer (daily-log-file nil))))
    (when (and dbuf (buffer-modified-p dbuf))
      ;; (message "[Debug] Daily log file:%s is saved." daily-log-file)
      (set-buffer dbuf)
      (basic-save-buffer))))

(defun daily-log-auto-save ()
  "Set time for auto saving daily-log file."
  (interactive)
  (setq-local
   daily-log-timer
   (run-with-idle-timer daily-log-interval t 'daily-log-save)))

(defun daily-log-auto-save-cancel ()
  "Cancel time for auto saving daily-log file."
  (interactive)
  (cancel-timer daily-log-timer))

(defun daily-log-open ()
  "Open daily-log file."
  (interactive)
  (find-file (daily-log-file nil))
  (daily-log-auto-save))

(provide 'daily-log)
;;; daily-log.el ends here
