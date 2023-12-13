;;; savehist-init.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: savehist-mode

;;; Commentary:


;;; Code:
(require 'savehist)
(require 'desktop)

(let ((newval (copy-sequence savehist-ignored-variables)))
  (mapc
   (lambda (var)
     (unless (member var newval)
       (push var newval)))
    '(
      load-history
      :prompt-history
      ))
  (unless (equal savehist-ignored-variables newval)
    (custom-set-variables `(savehist-ignored-variables ',newval))))

(defun savehist-init-other-saved-variables ()
  (let (othervars)
    (setq othervars
          (append othervars
                  (when desktop-save-mode
                    (append
                     desktop-globals-to-save
                     desktop-locals-to-save
                    ))
                  savehist-minibuffer-history-variables
                  savehist-ignored-variables
            ))))

(defun savehist-init-add-savehist-additional-variables (&optional file)
    "Set all history or ring variables,
except variables in `desktop-globals-to-save',
load-history and :prompt-history to savehist-additional-variables
so that variabels are saved to `savehist-file'."
    (let (addval newval histvars (inhibit-message 1) loadmsg)
      (setq histvars
            (apropos-internal "-\\(\\(history\\)\\|\\(ring\\)\\)\\'" 'boundp))
      (mapc (lambda (elt) (setq histvars (delete elt histvars)))
            (savehist-init-other-saved-variables))
      (mapc
       (lambda (elt)
         (unless (member elt savehist-additional-variables)
           (push elt addval)))
       histvars)
      (when addval
        (if file
            (setq loadmsg (format "After loading `%s', set in savehist-init; " file))
          (setq loadmsg "Set in savehist-init; "))
        (message (concat loadmsg (format "List variable savehist-additional-variables value is added: %s." addval)))
        (setq newval (copy-sequence savehist-additional-variables))
        (dolist (avl addval)
          (push avl newval))
        (custom-set-variables
         `(
           savehist-additional-variables
           ',newval
           nil nil ,loadmsg
           )))
      savehist-additional-variables))

(defun savehist-init-remove-savehist-additional-variables (&optional file)
  (ignore file)
  (let ((newval (copy-sequence savehist-additional-variables)))
    (mapc
     (lambda (elt) (setq newval (delete elt newval)))
     (append (savehist-init-other-saved-variables) savehist-ignored-variables))
    (unless (equal newval savehist-additional-variables)
      (custom-set-variables `(savehist-additional-variables ',newval)))))

(defun savehist-init-set-savehist-additional-variables (&optional file)
  (savehist-init-add-savehist-additional-variables file)
  (savehist-init-remove-savehist-additional-variables file)
  )

(mapc
 (lambda (ahook)
   (unless (memq 'savehist-init-set-savehist-additional-variables (eval ahook))
     (add-hook ahook 'savehist-init-set-savehist-additional-variables)))
 '(
   after-init-hook
   after-load-functions
   ))

(provide 'savehist-init)
;;; savehist-init.el ends here
