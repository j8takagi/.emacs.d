(require 'ediff)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(declare-function ediff-vc-internal "ediff-vers" (REV1 REV2 &optional STARTUP-HOOKS))

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

(provide 'init-ediff)
