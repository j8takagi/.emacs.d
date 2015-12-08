;;; skeleton-file-name.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'skeleton)

(define-skeleton skeleton-file-name
  "Insert a file name input in prompt."
  (let
      ((insert-default-directory nil))
    (read-file-name (concat "Insert file name relative path from " default-directory ": ")))
  str)

(define-skeleton skeleton-abs-file-name
  "Insert an absolute file name input in prompt."
  (read-file-name "Insert absolute file name: ")
  `(expand-file-name ,str))

(dolist
    (mapkeys
     '(
       ("C-c i" skeleton-file-name)
       ("C-c C-i" skeleton-abs-file-name)
       ))
  (let ((key (car mapkeys)) (func (nth 1 mapkeys)))
    (if (not (functionp func))
        (message "Warning: function `%s' is NOT defined." func)
      (global-set-key (kbd key) func))))

(provide 'skeleton-file-name)
;;; skeleton-file-name.el ends here
