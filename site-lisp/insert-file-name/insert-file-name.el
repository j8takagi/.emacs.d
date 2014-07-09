(defun insert-file-name (filename)
  "insert file name input in prompt."
  (interactive "*fInsert file name: ")
  (insert filename))

(defun insert-file-name-abs (filename)
  "insert absolute file name input in prompt."
  (interactive "*fInsert file name: ")
  (insert (expand-file-name filename)))

(provide 'insert-file-name)
