;;; view-mode-vi-bindings.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'view)

(defun goto-line-default-end (&optional line)
  "Go to LINE. if LINE is omitted, go to the end line."
  (interactive "P")
  (if line
      (forward-line  (- line (line-number-at-pos)))
    (goto-char (point-max)))
  (back-to-indentation))

(defun view-mode-vi-bindings-add ()
  (dolist
      (map
       '(
         ("$" move-end-of-line)
         ("/" isearch-forward)
         ("0" beginning-of-line)
         ("1" digit-argument)
         ("2" digit-argument)
         ("3" digit-argument)
         ("4" digit-argument)
         ("5" digit-argument)
         ("6" digit-argument)
         ("7" digit-argument)
         ("8" digit-argument)
         ("9" digit-argument)
         ("?" isearch-backward)
         ("B" backward-word)
         ("G" goto-line-default-end)
         ("SPC" scroll-up-command)
         ("b" scroll-down-command)
         ("h" backward-char)
         ("i" view-mode-disable)
         ("j" next-line)
         ("k" previous-line)
         ("l" forward-char)
         ("w" forward-word)
         ("RET" nil)
         ))
    (let ((key (car map)) (func (nth 1 map)))
      (if (and func (not (functionp func)))
          (message "%s is not defined." func)
      (define-key view-mode-map (kbd key) func)))))

(add-hook 'view-mode-hook 'view-mode-vi-bindings-add)

(provide 'view-mode-vi-bindings)
;;; view-mode-vi-bindings.el ends here
