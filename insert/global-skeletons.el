;;; global-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
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

(global-set-key (kbd "C-c i") 'skeleton-file-name)

(global-set-key (kbd "C-c C-i") 'skeleton-abs-file-name)

(provide 'global-skeletons)
;;; global-skeletons.el ends here
