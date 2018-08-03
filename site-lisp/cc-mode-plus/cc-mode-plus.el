(require 'cc-mode)

(defun init-cc-gtags-mode-on ()
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (gtags-mode 1)))

(defun init-cc-require-gnu-mp ()
  (when (derived-mode-p 'c-mode 'c++-mode)
    (require 'gnu-mp)))

(defun init-cc-disable-electric-state()
  (when (derived-mode-p 'c-mode)
    (c-toggle-electric-state -1)))

(provide 'cc-mode-plus)
