;;; listify-test.el

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'ert)
(require 'cus-edit)
(load-file "../listify.el")


(ert-deftest listify-test-update-or-add-alist-1 ()
  "Test `update-or-add-alist'.
update a value of quoted symbol key. "
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc 'b "B") '((1 . "a") (b . "B") ("c" . "c"))))
    ))

(ert-deftest listify-test-update-or-add-alist-2 ()
  "Test `update-or-add-alist'.
update a value of integer key."
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc 1 "A") '((1 . "A") (b . "b") ("c" . "c"))))
    ))

;;; listify-test.el ends here
