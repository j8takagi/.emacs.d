;;; my-init.el --- 

;; Copyright (C) 2016 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:
;;; Code:

(defun update-or-add-alist (alist-var key value)
  "If KEY in ALIST, update VALUE of the KEY.
Unless, cons cell (KEY . VALUE) is added."
  (interactive)
  (let (aconscell (alist (symbol-value alist-var)))
   (if (setq aconscell (assoc key alist))
       (unless (equal (cdr aconscell) value)
         (setf (cdr aconscell) value))
     (set alist-var (push (cons key value) alist)))
   alist))

(defvar system-name-simple
  (replace-regexp-in-string "\\..*\\'" "" (system-name))
  "The simple host name of the machine Emacs is running on, which is without domain information.")

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

(defun my-init-requires (features-list)
  "Require FEATURE, and the result is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (dolist (afeat features-list)
    (my-init-require afeat)))

(defun my-init-require-by-system (system-features-list)
  (dolist (syslib system-features-list)
    (when (equal (eval (car syslib)) (nth 1 syslib))
      (my-init-require (nth 2 syslib)))))

(defun my-init-add-package-archives (archives-list)
  "Add package archives to `package-archives'.
Each element of ARCHIVES-LIST has the form (ID LOCATION)."
  (dolist (aarch archives-list)
    (update-or-add-alist 'package-archives (car aarch) (cadr aarch))))

(defun my-init-install-package (pkg &optional pkg-from)
  "Check whether PKG is installed. When not installed, the installation begins.
If the package requires other packages, installation of the packges begin recursively.
This function returns the list of (`package' `required package')."
  (let (pkgs req-pkgs pkg-desc)
    (unless (package-installed-p pkg)
      (if pkg-from
          (message "Package `%s' required from `%s' is not installed." pkg pkg-from))
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

(defun my-init-check-package (req-pkg-list)
  "Check whether packages in REQ-PKG_LIST is installed, updated, or
packages not in REQ-PKG_LIST is installed."
  (let (pkgs real-pkgs update-pkgs)
    ;; If packages in REQ-PKG_LIST is not installed, install.
    (dolist (req-pkg req-pkg-list)
      (dolist (pkg (my-init-install-package req-pkg))
        (when (and (cadr pkg) (not (member (car pkg) pkgs)))
          (message "Package `%s' is required from `%s'." (car pkg) (cadr pkg)))
        (add-to-list 'pkgs (car pkg))))
    (message "Required packages - %s" req-pkg-list)
    (setq real-pkgs (mapcar 'car package-alist))
    (message "Installed packages - %s" (reverse real-pkgs))
    ;; updated packages
    (package-menu--refresh real-pkgs)
    (dolist (update-pkg (package-menu--find-upgrades))
      (message "Info: Package %s is updated. Version %s is available."
               (car update-pkg)
               (package-desc-version (cdr update-pkg))))
    ;; installed packages not in REQ-PKG-LIST
    (dolist (pkg pkgs)
      (setq real-pkgs (delete pkg real-pkgs)))
    (when real-pkgs
      (message "Info: Unexpected installed packages %s"  (reverse real-pkgs)))))

(defun my-init-set-autoloads (functions-list)
  "Define autoload functions from FUNCTIONS-LIST.
Each element of ARCHIVES-LIST has the form (FUNCTION FILE DOC)."
  (let (funcs)
    (dolist (afunc functions-list)
      (add-to-list
       'funcs
       (my-init-set-autoload (car afunc) (nth 1 afunc) (nth 2 afunc))))
    (if (not funcs)
        (message "Autoload functions is not set.")
      (message "Autoload functions - %s" (reverse funcs)))))

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
           (message "Warning: In setting autoload functions, fails to set autoload `%s' from `%s'.\n%s: %s"
                    function file (car err) (cadr err))))))
    res))

(defun my-init-global-set-key (key function)
  "Give KEY a global binding as FUNCTION by global-set-key.
If FUNCTION is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (if (not (fboundp function))
      (message "Warning: In setting keybind, function `%s' is void." function)
    (global-set-key (kbd key) function)))

(defun my-init-global-set-keys (keys-list)
  (dolist (mapkeys keys-list)
    (my-init-global-set-key (car mapkeys) (cadr mapkeys))))

(defun my-init-global-unset-keys (keys-list)
  (dolist (key keys-list)
    (global-unset-key (kbd key))))

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
               (message "Warning: In setting `%s' keybind, function `%s' is void."
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
                     "Warning: In setting `%s', function `%s' is not defined."
                     ,modemap-name func)
                  (define-key ,modemap (kbd key) func))))))
        `(add-hook ',hook ',func-init-keybind))))))

(defun my-init-modemap-set-keys (modekey-list)
  (dolist (modekey modekey-list)
    (my-init-modemap-set-key
     (car modekey) (nth 1 modekey) (nth 2 modekey) (nth 3 modekey))))

(defun my-init-set-mode (mode)
  "Set MODE. MODE format is assumed as `(FUNCTION 1)' to enable the mode, or `(FUNCTION 0)' to disable the mode. FUNCTION presents minor mode.
If FUNCTION in MODE is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (if (not (fboundp (car mode)))
      (message "Warning: In setting minor mode, function `%s' is void." (car mode))
    (eval mode)))

(defun my-init-set-modes (mode-list)
  (dolist (mode mode-list)
    (my-init-set-mode mode)))

(defun my-init-custom-set-default (var-list)
  (dolist (varval var-list)
    (custom-set-default (car varval) (cadr varval))))

(defun my-init-set-variables (var-list)
  (dolist (varval var-list)
    (set-variable (car varval) (cadr varval))))

(defun my-init-defalias (symbol-list)
  (dolist (symdef symbol-list)
    (let ((asym (car symdef)) (adef (cadr symdef)))
     (when (fboundp asym)
         (message "Info: Function `%s' is already defined." asym))
     (cond
      ((not (fboundp adef)) (message "Warning: In setting alias, symbol `%s' is not function." adef))
      (t
       (defalias asym adef)
       (message "`%s' is defined as alias of `%s'." asym adef))))))

(defun my-init-set-default-frame-alist (parameters-list)
  (dolist (fparam parameters-list)
    (update-or-add-alist 'default-frame-alist (car fparam) (cadr fparam)))
  (message "default-frame-alist: %s" default-frame-alist))

(defun my-init-add-completion-ignored-extensions (extensions-list)
  (dolist (ext extensions-list)
    (add-to-list 'completion-ignored-extensions ext)))

(defun my-init-set-display-buffer-same-window (buffer-pattern-list)
  (dolist (bufptn buffer-pattern-list)
    (update-or-add-alist 'display-buffer-alist
                         bufptn '(display-buffer-same-window))))

(defun my-init-set-magic-mode-alist (magic-list)
  (dolist (magic magic-list)
    (let (mode)
      (if (not (fboundp (setq mode (cadr magic))))
          (message "Warning: In setting magic-mode-alist, function `%s' is void." mode)
        (update-or-add-alist 'magic-mode-alist (car magic) mode)))))

(defun my-init-add-automode-alist (mode-list)
  (dolist (ptnmode mode-list)
    (let (mode)
      (if (not (fboundp (setq mode (cadr ptnmode))))
          (message "Warning: In setting auto-mode-alist, function `%s' is void." mode)
        (update-or-add-alist 'auto-mode-alist (car ptnmode) mode)))))

(defun my-init-overwrite-auto-mode-alist (mode-to mode-from)
  "Over write auto-mode-alist from one mode to anothor mode.
If MODE-TO or MODE-FROM is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (conscell)
    (cond
     ((not (fboundp mode-to))
      (message "Warning: In setting auto-mode-alist, mode `%s' overwritten to is void function." mode-to))
     ((not (fboundp mode-from))
      (message "Warning: In setting auto-mode-alist, mode `%s' overwritten from is void function." mode-from))
     (t
      (while (setq conscell (rassq mode-from auto-mode-alist))
        (setcdr conscell mode-to))))))

(defun my-init-overwrite-auto-mode-alists (mode-list)
  (dolist (mode-to-from mode-list)
    (my-init-overwrite-auto-mode-alist (car mode-to-from) (cadr mode-to-from))))

(defun my-init-set-hook (hook function)
  "Add FUNCTION to HOOK by add-hook function.
If FUNCTION or HOOK is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (cond
   ((not (boundp hook)) (message "Warning: In setting hooks, hook `%s' is void." hook))
   ((not (fboundp function)) (message "Warning: In setting hooks, function `%s' is void." function))
   (t
    (add-hook hook function))))

(defun my-init-set-hooks (hooks-list)
  "Add function to hooks by list of (FUNCTION HOOK)."
  (dolist (hookfunc hooks-list)
    (my-init-set-hook (car hookfunc) (cadr hookfunc))))

(defun my-init-setenv (env-list)
  (dolist (envval env-list)
    (setenv (car envval) (cadr envval))))

;; Emacs開始にかかった時間をメッセージに表示
(defun my-init-message-startup-time ()
  "Message Duration of the Emacs initialization time."
  (message "Duration of the Emacs initialization - %s" (emacs-init-time)))

(provide 'my-init)
;;; my-init.el ends here
