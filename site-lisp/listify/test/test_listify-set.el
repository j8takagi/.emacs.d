;;; test_listify-test.el

;; Copyright (C) 2017, 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords:

;;; Commentary:

;;; Code:
(require 'ert)
(require 'cus-edit)
(load-file "../listify.el")

(defgroup listify-test-set
  '((test-listify-int custom-variable))
  "Variable for testing listify."
  :group 'test)

(defcustom listify-test-list nil
  "List custom variable for testing listify."
  :group 'test-listify
  :type '(repeat sexp))

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

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; test_listify-test.el ends here