(require 'ediff)
(require 'vc)
(require 'ediff-vers)
(require 'dired)

;;;###autoload
(defun ediff-vc-latest-current ()
  "Run Ediff of buffer file by comparing the latest and current."
  (interactive)
  (let ((file) (state))
    (setq file (buffer-file-name))
    (unless file
      (error "buffer not visiting file"))
    (setq state (vc-state file))
    (if (member state '(up-to-date added))
        (message "%s: %s" file state)
      (ediff-load-version-control)
      (ediff-vc-internal "" ""))))

;;;###autoload
(defun find-file-revision (&optional file revision)
  "find-file FILE REVISION.
Input FILE first, REVISION then.
Or, input FILE as 'FILE.~REVISON~' and FILE and REVISION is specified."
  (interactive "P")
  (unless (stringp file)
    (setq file (expand-file-name (read-file-name "Find version controled file: "))))
  ;; find-file FILE REVISION by 'FILE.~REVISION~'."
  (when (string-match "\\(.+\\)\\.~\\(.+\\)~$" file)
    (setq revision (substring file (match-beginning 2) (match-end 2)))
    (setq file (substring file (match-beginning 1) (match-end 1))))
  (unless (vc-backend file)
    (error (format "%s is not under version control." file)))
  (unless (stringp revision)
    (setq revision
          (vc-read-revision
           (format
            "Revision (default %s's working revision): " (file-name-nondirectory file))
           (list file))))
  (when (string= revision "")
    (setq revision nil))
  (set-buffer (find-file-noselect file))
  (switch-to-buffer (vc-find-revision file revision)))

;;;###autoload
(defun dired-ediff-vc-latest-current ()
  "Run Ediff of file named on this line by comparing the latest version and current."
  (interactive)
  (let ((find-file-run-dired nil))
    (find-file (dired-get-file-for-visit))
    (ediff-vc-latest-current)))

;;;###autoload
(defun ediff-redisplay-current-frame ()
  "Display Ediff Control panel in the current frame"
  (interactive)
  (when (get-buffer "*Ediff Control Panel*")
    (switch-to-buffer "*Ediff Control Panel*")
    (delete-other-windows)
    (ediff-recenter)))

(defvar ediff-saved-window-configuration)

(defun ediff-save-window-configuration ()
  (setq ediff-saved-window-configuration (current-window-configuration)))

(defun ediff-restore-window-configuration ()
  (set-window-configuration ediff-saved-window-configuration))

(defun my-ediff-quit (reverse-default-keep-variants)
  "Finish an Ediff session and exit Ediff.
Unselects the selected difference, if any, restores the read-only and modified
flags of the compared file buffers, kills Ediff buffers for this session
\(but not buffers A, B, C\).

If `ediff-keep-variants' is nil, the user will be asked whether the buffers
containing the variants should be removed \(if they haven't been modified\).
If it is t, they will be preserved unconditionally.  A prefix argument,
temporarily reverses the meaning of this variable."
  (interactive "P")
  (let (buf)
    (ediff-barf-if-not-control-buffer)
    (setq buf (current-buffer))
    (ediff-really-quit reverse-default-keep-variants)
    (kill-buffer buf)))

(provide 'ediff-vc-plus)
