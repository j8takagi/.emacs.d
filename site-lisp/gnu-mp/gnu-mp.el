(defun find-gnu-mp-documents ()
  "Find documentation on GNU MP functions in programing C"
  '(let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
     (setcar (nthcdr 3 mode-value)
             (cons '("(gmp)Function Index" nil "^ -.* " "\\>")
                   (nth 3 mode-value)))))

(eval-after-load "info-look" '(find-gnu-mp-documents))

(provide 'gnu-mp)
