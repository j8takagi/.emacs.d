;;; vc-plus.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'ediff)
(require 'vc)
(require 'ediff-vers)
(require 'dired)




(defcustom vc-plus-delete-revision-buffer t
  "Non Nil means delete revision buffer after ediff quit."
  :type 'boolean
  :group 'ediff)

(defvar vc-plus-revision-buffer nil)

(defvar vc-plus-window-configuration nil)

(defun vc-plus-save-window-configuration ()
  (setq vc-plus-window-configuration (current-window-configuration)))

(defun vc-plus-restore-window-configuration ()
    (set-window-configuration vc-plus-window-configuration))

(add-hook 'ediff-before-setup-hook 'vc-plus-save-window-configuration)

(mapc
 (lambda (hook) (add-hook hook 'vc-plus-restore-window-configuration))
 '(
   ediff-suspend-hook
   ediff-quit-hook
   ))

;;;###autoload
(defun vc-plus-latest-current (&optional rev)
  "Run Ediff of buffer file by comparing revision REV and current.
If rev is omitted or nil, compare latest and current."
  (interactive)
  (let ((file) (state) (arev ""))
    (when rev
      (setq arev rev))
    (setq file (buffer-file-name))
    (unless file
      (error "buffer not visiting file"))
    (setq state (vc-state file))
    (if (member state '(up-to-date added))
        (message "%s: %s" file state)
      (ediff-load-version-control)
      (ediff-vc-internal arev ""))))

;;;###autoload
(defun vc-plus-find-file-revision (&optional file revision)
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
(defun vc-plus-dired-latest-current ()
  "Run Ediff of file named on this line by comparing the latest
  version and current."
  (interactive)
  (let ((find-file-run-dired nil))
    (find-file (dired-get-file-for-visit))
    (vc-plus-latest-current)))

;;;###autoload
(defun vc-plus-redisplay-current-frame ()
  "Display Ediff Control panel in the current frame"
  (interactive)
  (when (get-buffer "*Ediff Control Panel*")
    (switch-to-buffer "*Ediff Control Panel*")
    (delete-other-windows)
    (ediff-recenter)))

;;;###autoload
(defun vc-plus-quit (reverse-default-keep-variants)
  "Finish an Ediff session and exit Ediff.
Unselects the selected difference, if any, restores the read-only and modified
flags of the compared file buffers, kills Ediff buffers for this session
\(but not buffers A, B, C\)."
  (interactive "P")
  (let (ctlbuf)
    (ediff-barf-if-not-control-buffer)
    (setq ctlbuf (current-buffer))
    (ediff-really-quit reverse-default-keep-variants)
    (kill-buffer ctlbuf)))

(provide 'vc-plus)
;;; vc-plus.el ends here