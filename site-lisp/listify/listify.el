;;; listify.el --- Listify Emacs initialization files.

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: init

;;; Commentary:
;;; Code:
(require 'cus-edit)
(require 'package)

(defun update-or-add-alist (alist-var key value)
  "If KEY in ALIST, update VALUE of the KEY.
Unless, cons cell (KEY . VALUE) is added."
  (interactive)
  (let (acell (alst (symbol-value alist-var)))
   (if (setq acell (assoc key alst))
       (unless (equal (cdr acell) value)
         (setf (cdr acell) value))
     (set alist-var (push (cons key value) alst)))
   alst))

(defun overwrite-values-alist (alist-var value-new-old-alist)
  "Overwrite ALIST-VAR alist by VALUE-NEW-OLD alist.
Each VALUE-NEW-OLD-ALIST has the form (VALUE-NEW . VALUE-OLD)."
  (let ((alst (symbol-value alist-var)))
    (dolist (newold value-new-old-alist)
      (while (setq acell (rassoc (cdr newold) alst))
        (setf (cdr acell) (car newold))))
    alst))

(defun listify-validate-custom-variable-type (custom-variable &optional value)
  "Varidate VALUES is match CUSTOM-VARIABLE to custom-variable-type in symbol property.
If VALUE matches custom-variable-type in symbol properties list, t.
When VALUE is ommited or nil, current value of CUSTOM-VARIABLE is validated."
  (let (atype)
    (when (null value)
      (setq value (symbol-value custom-variable)))
    (when (setq atype (custom-variable-type custom-variable))
      (widget-apply (widget-convert atype) :match value))))

;; Emacs開始にかかった時間をメッセージに表示
(defun message-startup-time ()
  "Message Duration of the Emacs initialization time."
  (message "Duration of the Emacs initialization - %s" (emacs-init-time)))

(defvar system-name-simple
  (replace-regexp-in-string "\\..*\\'" "" (system-name))
  "The simple host name of the machine Emacs is running on, which is without domain information.")

(defun listify-requires (&rest feature)
  "Require FEATURE, and the result is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (feats)
    (dolist (afeat feature)
      (if (featurep afeat)
          (message "Info: Feature `%s' is already required." afeat)
        (if (not (locate-library (symbol-name afeat)))
            (message "Warning: Feature `%s' is NOT found." afeat)
          (if (null (require afeat nil 1))
              (message "Warning: Fails to require feature - %s" afeat)
            (setq feats (append feats (list afeat)))))))
    (message "Features are required - %s" feats)))

(defun listify-requires-by-system (&rest sys-features)
  "If current system type or window system got by VARIABLE is match to SYSTEM, Require FEATURE by `listify-requires' in SYS-FEATURES.
Each element of SYS-FEATURES has the form (VARIABLE SYSTEM FEATURE)."
  (dolist (asysfeat sys-features)
    (when (equal (eval (car asysfeat)) (cadr asysfeat))
      (listify-requires (nth 2 asysfeat)))))

(defun listify-packages-add-archives (&rest archives)
  "Add package archives to `package-archives'.
Each element of ARCHIVES has the form (ID LOCATION)."
  (dolist (aarch archives)
    (update-or-add-alist 'package-archives (car aarch) (cadr aarch))))

(defun listify-packages-install (pkg)
  (if (not (assq pkg package-archive-contents))
      (message "Warning: Package `%s' is NOT found on archives." pkg)
    (message "Installation of package `%s' begins." pkg)
    (condition-case err
        (package-install pkg)
      (error (message "Warining: Fails to install package `%s'.\n%s: %s" pkg (car err) (cadr err))))))

(defun listify-packages-required (pkg)
  (let (pkgdesc)
    (when (setq pkgdesc (assq pkg package-alist))
      (mapcar 'car (package-desc-reqs (cadr pkgdesc))))))

(defun listify-packages-list-dependent (pkg &optional pkg-from)
  "Check whether PKG is installed. When not installed, the installation begins.
If the package requires other packages, installation of the packges begin recursively.
This function returns the list of (`package' `required package')."
  (let (pkgs req-pkgs)
    (unless (package-installed-p pkg)
      (when pkg-from
        (message "Package `%s' required from `%s' is not installed." pkg pkg-from))
      (listify-packages-install pkg))
    (push (cons pkg pkg-from) pkgs)
    (dolist (req-pkg (listify-packages-required pkg))
      (dolist (rp (listify-packages-list-dependent req-pkg pkg))
        (push rp pkgs)))
    pkgs))

(defun listify-packages-message-update (pkgs)
  (package-menu--refresh pkgs)
  (dolist (pkg (package-menu--find-upgrades))
    (message "Info: Package %s is updated. Version %s is available."
             (car pkg) (package-desc-version (cdr pkg)))))

(defun listify-packages-message-unexpected (pkgs real-pkgs)
  (dolist (pkg pkgs)
    (setq real-pkgs (delete pkg real-pkgs)))
  (when real-pkgs
    (message "Info: Unexpected installed packages %s" real-pkgs)))

(defun listify-packages-check (&rest package)
  "Check the packages in PACKAGE, packages and dependent packages are
installed and updated, and unexpected packages are not installed."
  (let (pkgs deps real-pkgs update-pkgs)
    (message "Required packages - %s" package)
    (dolist (req-pkg package)
      (push req-pkg pkgs)
      (dolist (pkg (listify-packages-list-dependent req-pkg))
        (when (and (cdr pkg) (null (member (car pkg) pkgs)) (null (package-built-in-p (car pkg))))
          (push pkg deps)
          (push (car pkg) pkgs))))
    (message "Package and require package - %s" deps)
    (message "Installed packages - %s"
             (setq real-pkgs (nreverse (mapcar 'car package-alist))))
    ;; updated packages
    (listify-packages-message-update real-pkgs)
    ;; installed packages not in REQ-PKG-LIST
    (listify-packages-message-unexpected pkgs real-pkgs)
    pkgs))

(defun listify-autoloads-set (&rest func-file-doc)
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

(defun listify-set-alist (&rest alist-args)
  "Custom set ALIST-ARGS value to the alist.
Each ALIST-ARGS has the form (ALIST-NAME ((KEY1 VALUE1) (KEY2 VALUE2) ...)[ NOW[ REQUEST[ COMMENT]]])."
  (let (asym keyvals alsts)
    (dolist (alst alist-args)
      (setq asym (car alst) keyvals (symbol-value asym))
      (dolist (akeyval (cadr alst))
        (update-or-add-alist 'keyvals (car akeyval) (cadr akeyval)))
      (if (null (listify-validate-custom-variable-type asym keyvals))
          (message "%s: Variable type is mismatch.\nType: %s\nValue: %s"
                   asym (custom-variable-type asym) keyvals)
        (unless (eq keyvals (symbol-value asym))
          (custom-set-variables `(,asym ',keyvals ,(nth 2 alst) ,(nth 3 alst) ,(nth 4 alst)))
          (push asym alsts))))
    alsts))

(defun listify-set-list (&rest list-val)
  "Set LIST-VAL value to the list.
Each LIST-VAL has the form (LIST-NAME (VALUE1 VALUE2 ...)[ NOW[ REQUEST[ COMMENT]]])."
  (let (vals exps asym lsts)
    (dolist (lst list-val)
      (unless (listp (setq vals (cadr lst)))
        (error (format "The 2nd arg is not list.\nArg: %s" vals)))
      (setq asym (car lst) exps (symbol-value asym))
      (dolist (aval vals)
        (add-to-list 'exps aval))
      (if (null (listify-validate-custom-variable-type asym vals))
          (message "%s: Variable type is mismatch.\nType: %s\nValue: %s"
                   asym (custom-variable-type asym) exps)
        (unless (eq vals (symbol-value asym))
          (custom-set-variables `(,asym ',exps
                                        ,(nth 2 lst) ,(nth 3 lst) ,(nth 4 lst)))
          (push asym lsts))))
    lsts))

(defun listify-set (&rest args)
  "Custom set variable values specified in ARGS.
Each ARGS has form (SYMBOL EXP [NOW [REQUEST [COMMENT]]]).
The ARGS form is same to `custom-set-variables'.
Except EXP need no quote when EXP is SYMBOL,
and/or add each element when EXP is list,
update or add each element when EXP is association list (alist)."
  (let (asym aexp anow areq acomm vars)
    (dolist (arg args)
      (setq
       asym (nth 0 arg) aexp (nth 1 arg) anow (nth 2 arg) areq (nth 3 arg)
       acomm (listify-create-variable-comment asym (nth 4 arg)))
      (if (and (not (null aexp)) (listp aexp) (listp (cdr aexp)))
          (setq vars
                (append
                 vars
                 (funcall
                  (if (consp (car aexp)) 'listify-set-alist 'listify-set-list)
                  (list asym aexp anow areq acomm))))
        (if (null (listify-validate-custom-variable-type asym aexp))
            (message "%s: Variable type is mismatch.\nType: %s\nValue: %s"
                     asym (custom-variable-type asym) aexp)
          (when (not (equal (symbol-value asym) aexp))
            (custom-set-variables
             (list
              asym
              (if (and (not (null aexp)) (not (eq aexp t)) (or (symbolp aexp) (listp aexp))) `(quote ,aexp) aexp)
              anow areq acomm))
            (setq vars (append vars (list asym)))))))
    (listify-message-variables vars)))

(defun listify-message-variables(vars)
  (let (cusvars ovars)
    (dolist (avar vars)
      (if (custom-variable-p avar)
          (setq cusvars (append cusvars (list avar)))
        (setq ovars (append ovars (list avar)))))
    (message
     (concat
      (when cusvars (format "Custom variables are set. - %s" cusvars))
      (when (and cusvars ovars) "\n")
      (when ovars (format "Variables are set. - %s" ovars))))))

(defun listify-create-variable-comment (var &optional add-comment)
  "Create variable comment of VAR by loading file or buffer file and ADD-COMMENT."
  (let (acomm afile)
    (when (setq afile (or load-file-name buffer-file-name (buffer-name)))
      (setq acomm (concat (format "set in `%s'." afile))))
    (when add-comment
      (setq acomm (concat acomm (when acomm " ") add-comment)))
    (when (equal (listify-get-variable-comment var) acomm)
       (setq acomm nil))
     acomm))

(defun listify-get-variable-comment (var)
  (get var 'variable-comment))

(defun listify-defaliases (&rest sym-def)
  "Set SYMBOL’s function definition to DEFINITION in SYM-DEF.
Each SYM-DEF has the form (SYMBOL DEFINITION &optional DOCSTRING)."
  (let (asym adef)
    (dolist (asymdef sym-def)
     (when (fboundp (setq asym (car asymdef)))
       (message "Info: Function `%s' is already defined as %s." asym (indirect-function asym)))
     (if (not (fboundp (setq adef (cadr asymdef))))
         (message "Warning: In setting alias, symbol `%s' is not function." adef)
       (defalias asym adef (nth 3 asymdef))
       (message "`%s' is defined as alias of `%s'." asym adef)))))

(defun listify-global-set-keys (&rest key-func)
  "Give global binding as KEY-FUNC by global-set-key.
Each KEY-FUNC form is (KEY FUNCTION).

If FUNCTION is void, warning message is printed into the `*Messages' buffer,
or the standard error stream in batch mode."
  (let (afunc)
    (dolist (akeyfunc key-func)
      (if (not (fboundp (setq afunc (cadr akeyfunc))))
          (message "Warning: In setting keybind, function `%s' is void." afunc)
        (global-set-key (kbd (car akeyfunc)) afunc)))))

(defun listify-global-unset-keys (&rest keys)
  (dolist (akey keys)
    (global-unset-key (kbd akey))))

(defun listify-modemap-set-keys (&rest modemap)
  "Give KEY binding of MODEMAP as MAPKEYS after LIBRARY is loaded.
MODEMAP form is (MODEMAP LIBRARY HOOK ((KEY1 FUNCTION1) (KEY2 FUNCTION2) ... )).

If HOOK is not nil, the binding is via the HOOK.
If function in MAPKEYS is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (amap alib ahook amapname funcdef)
    (dolist (amodemap modemap)
      (setq
       amap (car amodemap) alib (cadr amodemap)
       ahook (nth 2 amodemap) keyfuncs (nth 3 amodemap)
       funcdef
       `(lambda ()
          (dolist (keyfunc ',keyfuncs)
            (let ((akey (car keyfunc)) (afunc (cadr keyfunc)))
              (if (not (fboundp afunc))
                  (message
                   ,(concat "Warning: In setting `" (setq amapname (symbol-name amap)) "' keybind, function `%s' is void.") afunc)
                (define-key ,amap (kbd akey) afunc))))))
      (eval-after-load alib
        (if (null ahook)
            (eval funcdef)
          (let ((func-add-keybind (read (concat "listify-" amapname "-keybind"))))
            (fset func-add-keybind funcdef)
            `(add-hook ',ahook ',func-add-keybind)))))))

(defun listify-set-modes (&rest modeval)
  "Set MODE. MODE format is assumed as `(FUNCTION 1)' to enable the mode, or `(FUNCTION 0)' to disable the mode. FUNCTION presents minor mode.

If FUNCTION in MODE is void, warning message is printed into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (amode)
    (dolist (amodeval modeval)
      (if (not (fboundp (setq amode (car amodeval))))
          (message "Warning: In setting minor mode, function `%s' is void." amode)
        (eval amodeval)))))

(defun listify-overwrite-auto-mode-alist (&rest mode-new-old)
  (let (anew aold alst)
    (dolist (newold mode-new-old)
      (cond
       ((not (fboundp (setq anew (car newold))))
        (message "Warning: In setting auto-mode-alist, mode `%s' overwritten from is void function." anew))
       ((not (fboundp (setq aold (cadr newold))))
        (message "Warning: In setting auto-mode-alist, mode `%s' overwritten to is void function." anew))
       (t
        (setq alst (push (cons anew aold) alst)))))
    (overwrite-values-alist 'auto-mode-alist alst)
    (message "auto-mode-alist is overwritten.")))

(defun listify-set-hooks (&rest hook-func)
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

(defun listify-setenv (&rest env-val)
  "Add system environment variables to values by list of ENV-VAL.
Each ENV-VAL form is (ENVIRONMENT VALUE)."
  (dolist (aenvval env-val)
    (setenv (car aenvval) (cadr aenvval))))

(provide 'listify)
;;; listify.el ends here
