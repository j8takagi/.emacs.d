;;; listify-test.el

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'ert)
(require 'cus-edit)
(load-file "../listify.el")


(ert-deftest listify-test-update-or-add-alist_nil ()
  "Test `update-or-add-alist'.
Insert a value to null alist."
  (let ((abc nil))
    (should (equal (update-or-add-alist 'abc 1 "a") '((1 . "a"))))
    ))

(ert-deftest listify-test-update-or-add-alist_same_element ()
  "Test `update-or-add-alist'.
Do not change when the element is a member of alist."
  (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
    (should (equal (update-or-add-alist 'abc 1 "a") '((1 . "a") (2 . "b") (3 . "c"))))
    ))

(ert-deftest listify-test-update-or-add-alist_same_car_element ()
  "Test `update-or-add-alist'.
Update alist when alist contains the element which car is same but cdr is different."
  (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
    (should (equal (update-or-add-alist 'abc 1 "A") '((1 . "A") (2 . "b") (3 . "c"))))
    ))

(ert-deftest listify-test-update-or-add-alist_same_car_cdr_element ()
  "Test `update-or-add-alist'.
Update alist when alist contains the element which car and cdr is same."
  (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
    (should (equal (update-or-add-alist 'abc 1 "a") '((1 . "a") (2 . "b") (3 . "c"))))
    ))

;;; listify-test.el ends here
