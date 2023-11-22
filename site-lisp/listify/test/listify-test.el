;;; listify-test.el -*- lexical-binding: t -*-

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'ert)
(require 'cus-edit)
(load-file "../listify.el")

(ert-deftest listify-test-update-cdrs-alist-1 ()
  "Tests `listify-update-cdrs-alist'.
overwrite a quoted symbol value. "
  (let ((abc '((1 . a) (2 . "a") (3 . 1))))
    (should
     (equal
      (listify-update-cdrs-alist abc '((b . a)))
      '((1 . b) (2 . "a") (3 . 1))))
    ))

(ert-deftest listify-test-update-cdrs-alist-2 ()
  "Tests `listify-update-cdrs-alist'.
Update cdr of a string value."
  (let ((abc '((1 . a) (2 . "a") (3 . 1))))
    (should
     (equal
      (listify-update-cdrs-alist abc '(("b" . "a")))
      '((1 . a) (2 . "b") (3 . 1))))
    ))

(ert-deftest listify-test-update-cdrs-alist-3 ()
  "Tests `listify-update-cdrs-alist'.
Update cdr of a integer value."
  (let ((abc '((1 . a) (2 . "a") (3 . 1))))
    (should
     (equal
      (listify-update-cdrs-alist abc '((11 . 1)))
      '((1 . a) (2 . "a") (3 . 11))))
    ))

(ert-deftest listify-test-validate-variable-1 ()
  "Tests `validate-custom-variable-type'.
Validate auto-mode-alist self value."
  (let ((auto-mode-alist '((".txt" . text-mode))))
    (should
     (listify-validate-variable 'auto-mode-alist))))

(ert-deftest listify-test-validate-variable-2 ()
  "Tests `validate-custom-variable-type'.
Validate auto-mode-alist self mismatch value."
  (let ((auto-mode-alist '(".txt" ".html")))
    (should
     (null
      (listify-validate-custom-variable-type 'auto-mode-alist)))))

(defgroup listify-test-set
  '((test-listify-int custom-variable))
  "Variable for testing listify."
  :group 'test)

(defcustom listify-test-int nil
  "Integer custom variable for testing listify."
  :group 'test-listify
  :type 'integer)

(defcustom listify-test-str nil
  "String custom variable for testing listify."
  :group 'test-listify
  :type 'string)

(defcustom listify-test-sym nil
  "Symbol custom variable for testing listify."
  :group 'test-listify
  :type 'symbol)

(defcustom listify-test-list nil
  "List custom variable for testing listify."
  :group 'test-listify
  :type '(repeat sexp))

(defcustom listify-test-alist nil
  "Association list custom variable for testing listify."
  :group 'test-listify
  :type '(alist))

(ert-deftest listify-test-set-cus-int ()
  "Test set integer to custom variable."
  (listify-set '(listify-test-int 12))
  (should (equal listify-test-int 12)))

(ert-deftest listify-test-set-cus-str ()
  "Test set integer to custom variable."
  (listify-set '(listify-test-str "abc"))
  (should (equal listify-test-str "abc")))

(ert-deftest listify-test-set-cus-sym ()
  "Test set integer to custom variable."
  (listify-set '(listify-test-sym car))
  (should (equal listify-test-sym 'car)))

(ert-deftest listify-test-set-cus-list ()
  "Test set integer to custom variable."
  (custom-set-variables '(listify-test-list nil))
  (listify-set '(listify-test-list (1 "a" "ab" 2 3 c)))
  (should (equal listify-test-list (nreverse '(1 "a" "ab" 2 3 c)))))

(ert-deftest listify-test-set-cus-list-add ()
  "Test set integer to custom variable."
  (custom-set-variables '(listify-test-list '(3 2 1)))
  (listify-set '(listify-test-list (4 5 6)))
  (should (equal listify-test-list '(6 5 4 3 2 1))))

(ert-deftest listify-test-set-cus-alist ()
  "Test set integer to custom variable."
  (custom-set-variables '(listify-test-alist nil))
  (listify-set '(listify-test-list ((1 "a") (2 "b") (3 "c"))))
  (should (equal listify-test-list '((3 . "c") (2 . "b") (1 . "a")))))

(ert-deftest listify-test-set-cus-alist-add ()
  "Test set integer to custom variable."
  (custom-set-variables '(listify-test-list '((3 . "c") (2 . "b") (1 . "a"))))
  (listify-set '(listify-test-list ((4 "d") (5 "e") (6 "f"))))
  (should (equal listify-test-list '((6 . "f") (5 . "e") (4 . "d") (3 . "c") (2 . "b") (1 . "a")))))

(ert-deftest listify-test-set-cus-alist-update ()
  "Test set integer to custom variable."
  (custom-set-variables '(listify-test-list '((3 . "c") (2 . "b") (1 . "a"))))
  (listify-set '(listify-test-list ((3 "d") (2 "e"))))
  (should (equal listify-test-list '((3 . "d") (2 . "e") (1 . "a")))))

;;; listify-test.el ends here
