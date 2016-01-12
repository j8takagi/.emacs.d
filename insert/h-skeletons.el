;;; h-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:

;;; Code:


(define-skeleton h-template
  "Template of C language header file."
  nil
  "#ifndef " (defvar h-template-file-name '(upcase (file-name-base (buffer-file-name)))) "_INCLUDE" n
  "#define " h-template-file-name "_INCLUDE" n
  n _ n n
  "#endif        /* end of " h-template-file-name "_INCLUDE */" n
  )

(provide 'h-skeletons)
;;; h-skeletons.el ends here
