;;; byte-compile-buffer-file.el

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(defun byte-compile-buffer-file(&optional load)
  "Compile visiting buffer file of Lisp code into a file of byte code."
  (interactive)
  (let ((filename (expand-file-name (buffer-file-name))))
    (if (not filename)
        (error "Buffer is not visiting a file.")
      (byte-compile-file filename))))

(provide 'byte-compile-buffer-file)
;;; byte-compile-buffer-file.el ends here
