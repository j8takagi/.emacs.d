;;; test_listify-set-alist.el --- 

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'ert)
(require 'cus-edit)
(load-file "../listify.el")


(ert-deftest listify-test-listify-set-alists_to_nil1 ()
  "Test `listify-set-alists'.
Insert a value to null alist."
  (should (equal
           (progn
             (custom-set-variables '(backup-directory-alist nil))
             (listify-set-alists
              'backup-directory-alist
              '(("." "~/backup")) nil nil "listify-set-alists test")
             backup-directory-alist)
           '(("." . "~/backup")))
          ))

(ert-deftest listify-test-listify-set-alists_to_nil2 ()
  "Test `listify-set-alists'.
Insert a value to null alist."
  (should (equal
           (progn
             (custom-set-variables
              '(backup-directory-alist nil)
              '(default-frame-alist nil))
             (listify-set-alists
              'backup-directory-alist
              '(
                ("." "~/backup")
                )
              nil nil "listify-set-alists test")
             (listify-set-alists
              'default-frame-alist
              '(
                (foreground-color "black")
                (background-color "gray99")
                (cursor-color "DarkOliveGreen")
                (cursor-type box)
                )
              nil nil "listify-set-alists test")
             default-frame-alist)
           '((cursor-type . box) (cursor-color . "DarkOliveGreen") (background-color . "gray99") (foreground-color . "black"))
           )))

;;; listify-test.el ends here


(provide 'test_listify-set-alists)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; test_listify-set-alists.el ends here
