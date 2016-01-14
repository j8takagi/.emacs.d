;;; my-init.el --- 

;; Copyright (C) 2016 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun my-init-require (feature)
  "Require FEATURE, and the result is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (if (featurep feature)
      (message "Info: Feature `%s' is already required." feature)
    (if (not (locate-library (symbol-name feature)))
        (message "Warning: Feature `%s' is NOT found." feature)
      (condition-case err
          (progn
            (require feature)
            (message "Feature `%s' is required." feature))
        (error (message "Warning: Fails to require feature `%s'.\n%s: %s" feature (car err) (cadr err)))))))

(defun my-init-install-package (pkg &optional pkg-from)
  "Check whether PKG is installed. When not installed, the installation begins.
If the package requires other packages, installation of the packges begin recursively.
This function returns the list of (`package' `required package')."
  (let (pkgs req-pkgs pkg-desc)
    (unless (package-installed-p pkg)
      (if pkg-from
          (message "Package %s required from %s is not installed." pkg pkg-from))
      (if (not (assq pkg package-archive-contents))
          (message "Warning: Package `%s' is NOT found on archives." pkg)
        (message "Installation of package `%s' begins." pkg)
        (condition-case err
            (package-install pkg)
          (error (message "Warining: Fails to install package `%s'.\n%s: %s" pkg (car err) (cadr err))))))
    (when (setq pkg-desc (assq pkg package-alist))
      (add-to-list 'pkgs `(,pkg ,pkg-from) 1)
      (dolist (req-pkg (mapcar 'car (package-desc-reqs (cadr pkg-desc))))
        (dolist (rp (my-init-install-package req-pkg pkg))
          (add-to-list 'pkgs rp 1))))
    pkgs))

(defun my-init-global-set-key (key function)
  "Give KEY a global binding as FUNCTION by global-set-key.
If FUNCTION is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (if (not (fboundp function))
      (message "Warning: In setting keybind, function `%s' is void." function)
    (global-set-key (kbd key) function)))

(defun my-init-modemap-set-key (library hook modemap mapkeys)
  "Give KEY binding of MODEMAP as MAPKEYS after LIBRARY is loaded.
If HOOK is not nil, the binding is via the HOOK.
If function in MAPKEYS is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (eval-after-load library
    (cond
     ((null hook)
      `(dolist (map ',mapkeys)
         (let ((key (car map)) (func (nth 1 map)))
           (if (not (fboundp func))
               (message "Warning: In setting %s keybind, function `%s' is void."
                        ,modemap func)
             (define-key ,modemap (kbd key) func)))))
     (t
      (let* ((modemap-name (symbol-name modemap))
             (func-init-keybind (read (concat "my-init-" modemap-name "-keybind"))))
        (fset
         func-init-keybind
         `(lambda ()
            (dolist
                (map ',mapkeys)
              (let ((key (car map)) (func (nth 1 map)))
                (if (not (functionp func))
                    (message
                     "Warning: In setting %s, function `%s' is not defined."
                     ,modemap-name func)
                  (define-key ,modemap (kbd key) func))))))
        `(add-hook ',hook ',func-init-keybind))))))

(defun my-init-set-fontfamily (charset fontfamily)
  "Use FONTFAMILY for character set CHARSET."
  (cond
   ((not (member charset charset-list))
    (message "Warning: character set %s is not defined." charset))
   ((not (member fontfamily (font-family-list)))
    (message "Warning: font family %s is not available." fontfamily))
   ((set-fontset-font t charset (font-spec :family fontfamily)))))

(defun my-init-set-autoload (function file doc)
  "Define FUNCTION to autoload from FILE by autoload function.
If FUNCTION is void or FILE is not found, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (res)
    (if (not (locate-library file))
        (message "Warning: In setting autoload functions, library file `%s' autoloaded from `%s' is not found." file function)
      (if (fboundp function)
          (message "Info: In setting autoload functions, function `%s' is already defined." function)
        (condition-case err
            (setq res (autoload function file doc 1))
          (error
           (message "Warning: In setting autoload functions, fails to set autoload %s from %s.\n%s: %s"
                    function file (car err) (cadr err))))))
    res))

(defun my-init-set-hook (hook function)
  "Add FUNCTION to HOOK by add-hook function.
If FUNCTION or HOOK is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (cond
   ((not (boundp hook)) (message "Warning: In setting hooks, hook `%s' is void." hook))
   ((not (fboundp function)) (message "Warning: In setting hooks, function `%s' is void." function))
   (t
    (add-hook hook function))))
(provide 'my-init)
;;; my-init.el ends here
