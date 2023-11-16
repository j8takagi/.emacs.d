;;; toggle-skeleton-pair.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'skeleton)

(defun toggle-skeleton-pair (arg)
  "Toggle pair insert.

See ‘skeleton-pair’ for pair insert.

If argument is omitted or nil, toggle pair insert,
If the prefix argument is positive, enable pair insert,
and if it is zero or negative, disable pair insert."
  (interactive "P")
  (if (null arg)
      (custom-set-variables `(skeleton-pair ,(not (symbol-value 'skeleton-pair))))
    (if (> arg 0)
        (custom-set-variables `(skeleton-pair t))
      (custom-set-variables `(skeleton-pair t))))
  (if skeleton-pair
      (message "skeleton-pair is enable.")
    (message "skeleton-pair is unable."))
  skeleton-pair)

(provide 'toggle-skeleton-pair)
;;; toggle-skeleton-pair.el ends here
