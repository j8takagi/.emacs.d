;;; test_listify-set-alist.el --- 

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'ert)
(require 'cus-edit)
(load-file "../listify.el")


(ert-deftest listify-test-listify-set-alists_nil ()
  "Test `listify-set-alists'.
Insert a value to null alist."
  (let ((abc nil))
    (should (equal
             (progn
               (listify-set-alists 'abc 1 "a")
               abc)
             '((1 . "a"))))
    ))

(ert-deftest listify-test-listify-set-alists_same_element ()
  "Test `listify-set-alists'.
Do not change when the element is a member of alist."
  (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
    (should (equal (listify-set-alists 'abc 1 "a") '((1 . "a") (2 . "b") (3 . "c"))))
    ))

(ert-deftest listify-test-listify-set-alists_same_car_element ()
  "Test `listify-set-alists'.
Update alist when alist contains the element which car is same but cdr is different."
  (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
    (should (equal (listify-set-alists 'abc 1 "A") '((1 . "A") (2 . "b") (3 . "c"))))
    ))

(ert-deftest listify-test-listify-set-alists_same_car_cdr_element ()
  "Test `listify-set-alists'.
Update alist when alist contains the element which car and cdr is same."
  (let ((abc '((1 . "a") (2 . "b") (3 . "c"))))
    (should (equal (listify-set-alists 'abc 1 "a") '((1 . "a") (2 . "b") (3 . "c"))))
    ))

;;; listify-test.el ends here


(provide 'test_listify-set-alists)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; test_listify-set-alists.el ends here
