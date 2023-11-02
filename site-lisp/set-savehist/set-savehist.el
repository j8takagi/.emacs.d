;;; set-savehist.el ---

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: savehist-mode

;;; Commentary:


;;; Code:
(require 'savehist)

(defun set-savehist-additional-variables (&optional file)
    "Set all history/ring variables,
except variables in `desktop-globals-to-save',
load-history and :prompt-history to savehist-additional-variables
so that variabels are saved to `savehist-file'."
    (let (histvars)
      (setq histvars
            (apropos-internal "-\\(\\(history\\)\\|\\(ring\\)\\)$" 'boundp))
      (mapc
       (lambda (elt)
         (setq histvars
               (delete elt histvars)))
       (append
        savehist-minibuffer-history-variables
        (when (boundp 'desktop-globals-to-save)
          desktop-globals-to-save)
        '(:prompt-history load-history)
        ))
      (mapc
       (lambda (elt)
         (unless (memq elt savehist-additional-variables)
           (push elt savehist-additional-variables)))
       histvars)
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
