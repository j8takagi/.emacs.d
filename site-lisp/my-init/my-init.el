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

(defun my-init-requires (&rest feature)
  "Require FEATURE, and the result is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (dolist (afeat feature)
    (if (featurep afeat)
        (message "Info: Feature `%s' is already required." afeat)
      (if (not (locate-library (symbol-name afeat)))
          (message "Warning: Feature `%s' is NOT found." afeat)
        (condition-case err
            (progn
              (require afeat)
              (message "Feature `%s' is required." afeat))
          (error (message "Warning: Fails to require feature `%s'.\n%s: %s" afeat (car err) (cadr err))))))))

(defun my-init-requires-by-system (&rest sys-features)
  "If current system type or window system got by VARIABLE is match to SYSTEM, Require FEATURE by `my-init-requires' in SYS-FEATURES.
Each element of SYS-FEATURES has the form (VARIABLE SYSTEM FEATURE)."
  (dolist (asysfeat sys-features)
    (when (equal (eval (car asysfeat)) (cadr asysfeat))
      (my-init-requires (nth 2 asysfeat)))))

(defun my-init-add-package-archives (archives-list)
  "Add package archives to `package-archives'.
Each element of ARCHIVES-LIST has the form (ID LOCATION)."
  (dolist (aarch archives-list)
    (update-or-add-alist 'package-archives (car aarch) (cadr aarch))))

(defun my-init-install-packages (pkg &optional pkg-from)
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
        (dolist (rp (my-init-install-packages req-pkg pkg))
          (add-to-list 'pkgs rp 1))))
    pkgs))

(defun my-init-check-packages (&rest package)
  "Check whether PACKAGE is installed, updated, or
packages not in PACKAGE is installed."
  (let (pkgs real-pkgs update-pkgs)
    ;; If PACKAGE is not installed, install.
    (dolist (req-pkg package)
      (dolist (pkg (my-init-install-packages req-pkg))
        (when (and (cadr pkg) (not (member (car pkg) pkgs)))
          (message "Package `%s' is required from `%s'." (car pkg) (cadr pkg)))
        (add-to-list 'pkgs (car pkg))))
    (message "Required packages - %s" package)
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

(defun my-init-set-autoloads (&rest func-file-doc)
  "Define autoload functions from FUNC-FILE-DOC.
Each FUNC-FILE-DOC has the form (FUNCTION FILE DOC).

If FUNCTION is void or FILE is not found, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (funcs ffd)
    (dolist (ffd func-file-doc)
      (let ((afunc (car ffd)) (afile (cadr ffd)) (adoc (nth 2 ffd)))
        (if (not (locate-library afile))
            (message "Warning: In setting autoload functions, library file `%s' autoloaded from `%s' is not found." afile afunc)
          (if (fboundp afunc)
              (message "Info: In setting autoload functions, function `%s' is already defined." afunc)
            (condition-case aerr
                (push (autoload afunc afile adoc 1) funcs)
              (error
               (message "Warning: In setting autoload functions, fails to set autoload `%s' from `%s'.\n%s" afunc afile aerr)))))))
    (if (not funcs)
        (message "Autoload functions is not defined.")
      (message "Autoload functions are defined. - %s" (reverse funcs)))))

(defun my-init-set-default-variables (&rest var-val)
  "Set default values to variables in VAR-VAL.
Each VAR-VAL has the form (VARIABLE VALUE)."
  (dolist (avarval var-val)
    (set-default (car avarval) (cadr avarval))))

(defun my-init-set-variables (&rest var-val)
  "Set variables in VAR-VAL.
Each VAR-VAL has the form (VARIABLE VALUE)."
  (let (avar)
    (dolist (avarval var-val)
      (if (not (setq avar (car avarval)))
          (message "Variable %s is not defined." avar)
        (set-variable avar (cadr avarval))))))

(defun my-init-set-alist (&rest alist-key-val)
  "Set ALIST-KEY-VAL value to the alist.
Each ALIST-KEY-VAL has the form (ALIST-NAME (KEY1 VALUE1) (KEY2 VALUE2) ...)."
  (let (asym)
  (dolist (aalist alist-key-val)
    (setq asym (car aalist))
    (dolist (akeyval (cdr aalist))
      (update-or-add-alist asym (car akeyval) (cadr akeyval)))
    (message "%s alist is set." (symbol-name asym)))))

(defun my-init-set-list (&rest list-val)
  "Set LIST-VAL value to the list.
Each LIST-VAL has the form (LIST-NAME (VALUE1 VALUE2 ...))."
  (let (asym)
    (dolist (lst list-val)
      (dolist (val (cadr lst))
        (add-to-list (setq asym (car lst)) val))
      (message "%s list is set." (symbol-name asym)))))


(defun my-init-defaliases (&rest sym-def)
  "Set SYMBOL’s function definition to DEFINITION in SYM-DEF.
Each SYM-DEF has the form (SYMBOL DEFINITION &optional DOCSTRING)."
  (let (asym adef)
    (dolist (asymdef sym-def)
     (when (fboundp (setq asym (car asymdef)))
       (message "Info: Function `%s' is already defined as `%s'." asym (symbol-name asym)))
     (if (not (fboundp (setq adef (cadr asymdef))))
         (message "Warning: In setting alias, symbol `%s' is not function." adef)
       (defalias asym adef (nth 3 asymdef))
       (message "`%s' is defined as alias of `%s'." asym adef)))))

(defun my-init-view-mode-buffer (&rest buffer-name-regexp)
  "Buffers match BUFFER-NAME-REGEXP to enable view-mode."
  (save-excursion
    (dolist (abuf (buffer-list))
      (dolist (aregexp buffer-name-regexp)
        (when (string-match-p aregexp (buffer-name abuf))
          (set-buffer abuf)
          (view-mode))))))

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

(defun my-init-modemap-set-keys (&rest modemap)
  "Give KEY binding of MODEMAP as MAPKEYS after LIBRARY is loaded.
MODEMAP form is (MODEMAP LIBRARY HOOK ((KEY1 FUNCTION1) (KEY2 FUNCTION2) ... )).

If HOOK is not nil, the binding is via the HOOK.
If function in MAPKEYS is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (amap alib ahook amapname funcdef)
    (dolist (amodemap modemap)
      (setq
       amap (car amodemap) alib (cadr amodemap) ahook (nth 2 amodemap)
       keyfuncs (nth 3 amodemap))
      (eval-after-load alib
        (progn
          (setq funcdef
                `(lambda ()
                   (dolist (keyfunc ',keyfuncs)
                     (let ((akey (car keyfunc)) (afunc (cadr keyfunc)))
                       (if (not (fboundp afunc))
                           (message
                            ,(concat "Warning: In setting `" (setq amapname (symbol-name amap)) "' keybind, function `%s' is void.") afunc)
                         (define-key ,amap (kbd akey) afunc))))))
          (cond
           ((not ahook)
            (eval funcdef))
           (t
            (let ((func-add-keybind (read (concat "my-init-" amapname "-keybind"))))
              (fset func-add-keybind funcdef)
              `(add-hook ',ahook ',func-add-keybind)))))))))

(defun my-init-set-modes (&rest modeval)
  "Set MODE. MODE format is assumed as `(FUNCTION 1)' to enable the mode, or `(FUNCTION 0)' to disable the mode. FUNCTION presents minor mode.

If FUNCTION in MODE is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (amode)
    (dolist (amodeval modeval)
      (if (not (fboundp (setq amode (car amodeval))))
          (message "Warning: In setting minor mode, function `%s' is void." amode)
        (eval amodeval)))))

(defun my-init-add-completion-ignored-extensions (extensions-list)
  (dolist (ext extensions-list)
    (add-to-list 'completion-ignored-extensions ext)))

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

(defun my-init-overwrite-auto-mode-alists (&rest mode-to-from)
  "Over write auto-mode-alist from one mode to anothor mode.
If MODE-TO or MODE-FROM is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (ato afrom (conscell))
    (dolist (atofrom mode-to-from)
      (cond
       ((not (fboundp (setq ato (car atofrom))))
        (message "Warning: In setting auto-mode-alist, mode `%s' overwritten to is void function." ato))
       ((not (fboundp (setq afrom (cadr atofrom))))
        (message "Warning: In setting auto-mode-alist, mode `%s' overwritten from is void function." afrom))
       (t
        (while (setq conscell (rassq afrom auto-mode-alist))
          (setcdr conscell ato)))))))

(defun my-init-set-hooks (&rest hook-func)
  "Add function to hooks by list of (FUNCTION HOOK).
If FUNCTION or HOOK is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (ahook afunc)
    (dolist (ahookfunc hook-func)
      (cond
       ((not (boundp (setq ahook (car ahookfunc))))
        (message "Warning: In setting hooks, hook `%s' is void." ahook))
       ((not (fboundp (setq afunc (cadr ahookfunc))))
        (message "Warning: In setting hooks, function `%s' is void." afunc))
       (t
        (add-hook ahook afunc))))))

(defun my-init-setenv (env-list)
  (dolist (envval env-list)
    (setenv (car envval) (cadr envval))))

;; Emacs開始にかかった時間をメッセージに表示
(defun my-init-message-startup-time ()
  "Message Duration of the Emacs initialization time."
  (message "Duration of the Emacs initialization - %s" (emacs-init-time)))

(provide 'my-init)
;;; my-init.el ends here
