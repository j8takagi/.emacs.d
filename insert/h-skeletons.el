;;; h-skeletons.el --- 

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:

;;; Code:
(let (v1 (upcase (file-name-base (buffer-file-name))))
  (define-skeleton h-template
    "Template of C language header file."
    "#ifndef " v1 "_INCLUDE" n
    "#define " v1 "_INCLUDE" n
    n _ n n
    "#endif        /* end of " v1 "_INCLUDE */" n
    ))

(define-auto-insert "\\.h\\'" 'h-template)

(provide 'h-skeletons)
;;; h-skeletons.el ends here
