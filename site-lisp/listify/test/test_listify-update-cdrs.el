;;; test_listify-update-cdrs.el ---

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:

;;; Code:
(require 'ert)
(load-file "../listify.el")


(ert-deftest test_listify-update-cdrs_1 ()
  "Test `overwrite_values_alist'.
case 1"
  (should
   (equal
    (let ((ab '((1 "a") (2 "b") (3 "a") (4 "b"))))
      (listify-update-cdrs ab '(("A" "a"))))
    '((1 "A") (2 "b") (3 "A") (4 "b"))
    )))

(ert-deftest test_listify-update-cdrs_2 ()
  "Test `overwrite_values_alist'.
case 2"
  (should
   (equal
    (let ((ab '((1 "a") (2 "b") (3 "a") (4 "b"))))
      (listify-update-cdrs ab '(("A" "a") ("B" "b"))))
    '((1 "A") (2 "B") (3 "A") (4 "B"))
    )))

(ert-deftest test_listify-update-cdrs_3 ()
  "Test `overwrite_values_alist'.
case 3: original variable is not changed."
  (should
   (equal
    (let ((ab '((1 "a") (2 "b") (3 "a") (4 "b"))))
      (princ (listify-update-cdrs ab '(("A" "a"))))
      ab)
    '((1 "a") (2 "b") (3 "a") (4 "b"))
    )))
(provide 'test_listify-update-cdrs)
;;; test_listify-update-cdrs.el ends here
