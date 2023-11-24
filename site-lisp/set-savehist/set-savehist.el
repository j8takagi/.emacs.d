;;; set-savehist.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: savehist-mode

;;; Commentary:


;;; Code:
(require 'savehist)

(defun set-savehist-additional-variables (&optional file)
    "Set all history or ring variables,
except variables in `desktop-globals-to-save',
load-history and :prompt-history to savehist-additional-variables
so that variabels are saved to `savehist-file'."
    (let ((addval nil) (histvars nil) (inhibit-message 1) (loadmsg nil))
      (setq histvars
            (apropos-internal "-\\(\\(history\\)\\|\\(ring\\)\\)\\'" 'boundp))
      (mapc
       (lambda (elt)
         (setq histvars
               (delete elt histvars)))
       (append
        savehist-minibuffer-history-variables
        (when (and desktop-save-mode (boundp 'desktop-globals-to-save))
          desktop-globals-to-save)
        '(:prompt-history load-history)
        ))
      (mapc
       (lambda (elt)
         (unless (member elt savehist-additional-variables)
           (push elt addval)))
       histvars)
      (when addval
        (if file
          (setq loadmsg (format "After loading `%s', set in set-savehist; " file))
        (setq loadmsg "Set in set-savehist; "))
        (message (concat loadmsg (format "List variable savehist-additional-variables value is added: %s." addval)))
        (custom-set-variables
         `(
           savehist-additional-variables
           ',(append addval savehist-additional-variables)
           nil nil ,loadmsg
           )))
      savehist-additional-variables))

(mapc
 (lambda (ahook)
   (unless (memq 'set-savehist-additional-variables (eval ahook))
     (add-hook ahook 'set-savehist-additional-variables)))
 '(
   after-init-hook
   after-load-functions
   ))

(provide 'set-savehist)
;;; set-savehist.el ends here
