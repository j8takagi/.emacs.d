;;; listify-test.el

;; Copyright (C) 2017-2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'ert)
(require 'cus-edit)
(load-file "../listify.el")


(ert-deftest listify-test-listify-add-or-update-alist_nil ()
  "Test `listify-add-or-update-alist'.
Insert a value to null alist."
  (should
   (equal
    (let ((abc nil))
      (listify-add-or-update-alist abc 1 "a"))
    '((1 . "a"))
    )))

(ert-deftest listify-test-listify-add-or-update-alist_same_element ()
  "Test `listify-add-or-update-alist'.
Do not change when the element is a member of alist."
  (should
   (equal
    (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
      (listify-add-or-update-alist abc 1 "a"))
    '((1 . "a") (2 . "b") (3 . "c"))
    )))

(ert-deftest listify-test-listify-add-or-update-alist_same_car_element ()
  "Test `listify-add-or-update-alist'.
Update alist when alist contains the element which car is same but cdr is different."
  (should
   (equal
    (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
      (listify-add-or-update-alist abc 1 "A"))
    '((1 . "A") (2 . "b") (3 . "c"))
     )))

(ert-deftest listify-test-listify-add-or-update-alist_different_car_element ()
  "Test `listify-add-or-update-alist'.
Insert element to the alist when alist not contains an element which car is same."
  (should
   (equal
    (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
      (listify-add-or-update-alist abc 4 "d"))
    '((4 . "d") (1 . "a") (2 . "b") (3 . "c"))
    )))

;;; test_listify-add-or-update-alist.el ends here
