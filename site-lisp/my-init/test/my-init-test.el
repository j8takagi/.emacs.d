;;; my-init-test.el --- 

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'ert)
(load-file "../my-init.el")


(ert-deftest my-init-test-update-or-add-alist-1 ()
  "Test `update-or-add-alist'.
update a value of quoted symbol key. "
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc 'b "B") '((1 . "a") (b . "B") ("c" . "c"))))
    ))

(ert-deftest my-init-test-update-or-add-alist-2 ()
  "Test `update-or-add-alist'.
update a value of integer key."
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc 1 "A") '((1 . "A") (b . "b") ("c" . "c"))))
    ))

(ert-deftest my-init-test-update-or-add-alist-3 ()
  "Test `update-or-add-alist'.
update a value of string key."
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc "c" "C") '((1 . "a") (b . "b") ("c" . "C"))))
    ))

(ert-deftest my-init-test-update-or-add-alist-4 ()
  "Test `update-or-add-alist'.
add a quoted symbol."
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc 4 'D) '((4 . D) (1 . "a") (b . "b") ("c" . "c"))))
    ))

(ert-deftest my-init-test-update-or-add-alist-5 ()
  "Test `update-or-add-alist'.
add a string value."
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc "d" "D") '(("d" . "D") (1 . "a") (b . "b") ("c" . "c"))))
    ))

(ert-deftest my-init-test-update-or-add-alist-6 ()
  "Test `update-or-add-alist'.
add an integer."
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc 4 4) '((4 . 4) (1 . "a") (b . "b") ("c" . "c"))))
    ))

(ert-deftest my-init-test-update-or-add-alist-7 ()
  "Test `update-or-add-alist'.
key and value is same to a cell."
  (let ((abc '((1 . "a") (b . "b") ("c" . "c"))))
    (should (equal (update-or-add-alist 'abc 1 "a") '((1 . "a") (b . "b") ("c" . "c"))))
    ))

(ert-deftest my-init-test-overwrite-value-alist-1 ()
  "Tests `overwrite-alist'.
overwrite a quoted symbol value. "
  (let ((abc '((1 . a) (2 . "a") (3 . 1))))
    (should (equal (overwrite-values-alist 'abc '((b . a))) '((1 . b) (2 . "a") (3 . 1))))
    ))

(ert-deftest my-init-test-overwrite-value-alist-2 ()
  "Tests `overwrite-alist'.
overwrite a string value."
  (let ((abc '((1 . a) (2 . "a") (3 . 1))))
    (should (equal (overwrite-values-alist 'abc '(("b" . "a"))) '((1 . a) (2 . "b") (3 . 1))))
    ))

(ert-deftest my-init-test-overwrite-value-alist-3 ()
  "Tests `overwrite-alist'.
overwrite a integer value."
  (let ((abc '((1 . a) (2 . "a") (3 . 1))))
    (should (equal (overwrite-values-alist 'abc '((11 . 1))) '((1 . a) (2 . "a") (3 . 11))))
    ))

(provide 'my-init-test)
;;; my-init-test.el ends here