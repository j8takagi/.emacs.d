(require 'cc-mode)

(custom-set-variables
 '(c-default-style "k&r")
 '(c-basic-offset 4))

(defun init-cc-ggtags-mode-turnon ()
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (ggtags-mode 1)))

(defun init-require-gnu-mp ()
  (when (derived-mode-p 'c-mode 'c++-mode)
    (require 'gnu-mp)))

(defun init-c-disable-electric-state()
  (when (derived-mode-p 'c-mode)
    (c-toggle-electric-state -1)))

(dolist                            ; c-mode-common-hookに追加する関数
    (func
     '(
       init-cc-ggtags-mode-turnon
       init-require-gnu-mp
       init-c-disable-electric-state
       ))
  (add-hook 'c-mode-common-hook func))

(provide 'init-cc-mode)
