(require 'cc-mode)

(custom-set-variables
 '(c-default-style "k&r")
 '(c-basic-offset 4))

(defun init-cc-ggtags-mode-on ()
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (ggtags-mode 1)))

(defun init-cc-require-gnu-mp ()
  (when (derived-mode-p 'c-mode 'c++-mode)
    (require 'gnu-mp)))

(defun init-cc-disable-electric-state()
  (when (derived-mode-p 'c-mode)
    (c-toggle-electric-state -1)))

(my-init-custom-set-list
 '(c-mode-common-hook
   (
    init-cc-ggtags-mode-on
    init-cc-require-gnu-mp
    init-cc-disable-electric-state
    )))

(provide 'init-cc-mode)
