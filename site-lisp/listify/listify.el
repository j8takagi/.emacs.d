;;; listify.el -*- lexical-binding: t -*-
;; Copyright (C) 2017-2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: init

;;; Commentary:
;;; Listify enable to use simple lists to edit Emacs init files.

;;; Code:
(require 'custom)
(require 'cus-edit)
(require 'wid-edit)

(defgroup listify nil
  "Emacs start-up utilities."
  :prefix "listify-"
  :group 'initialization
  :group 'convenience
  )

(defcustom listify-inhibit-echo t
  "Non-nil means calls to `listify-message' are
not displaied in echo area, but still logged to
*Messages* buffer."
  :group 'listify
  :type 'boolean
  )

(defcustom listify-inhibit-message nil
  "Non-nil means calls to `listify-message' are
not displaied in echo area and not logged to
*Messages* buffer."
  :group 'listify
  :type 'boolean
  )

(defvar listify-init-set-variables nil
  "Variables set in listify functions,
mainly from initialization files."
)

(defvar listify-system-name-simple
  (replace-regexp-in-string "\\..*\\'" "" (system-name))
  "The simple host name of the machine Emacs is running on,
which is without domain information.")

(defun listify-message (format-string &rest args)
  "Display a message using `message' if `listify-inhibit-message' is nil.
If `listify-inhibit-echo' is t, the message is not dispalayed in echo area,
and it is still logged to the *Meaasge* buffer."
  (let ((inhibit-message listify-inhibit-echo))
    (unless (or noninteractive listify-inhibit-message)
      (apply #'message format-string args))))

(defun listify-add-or-update-alist (alist key value)
  "return ALIST which value is updated or added.
If KEY exists in ALIST, update VALUE of the KEY.
Unless, cons cell (KEY . VALUE) is added."
  (let ((newalist (copy-alist alist)) (acell nil))
    (if (setq acell (assoc key newalist))
        (unless (equal (cdr acell) value)
          (setcdr acell value))
      (setq newalist (push (cons key value) newalist)))
    newalist))

(defun listify-update-cdrs-alist (alist cdrs-new-old)
  "Update cdrs of ALIST using CDRS-NEW-OLD alist.
Arguments has the form:

    SYM ((CDR-NEW1 . CDR-OLD1) (CDR-NEW2 . CDR-OLD2)) ..."
  (let ((newalist (copy-alist alist)) (old nil) (acell nil))
    (dolist (newold cdrs-new-old)
      (if (listp (cdr newold))
          (setq old (cadr newold))
        (setq old (cdr newold)))
      (while (setq acell (rassoc old newalist))
        (setcdr acell (car newold))))
    newalist))

(defun listify-get-variable-comment (var)
  (get var 'variable-comment))

(defun listify-message-variable-comment (var)
  (interactive "vVariable name: ")
  (listify-message (listify-get-variable-comment var)))

(defun listify-setting-file ()
  "Return the file name setting varables."
  (or load-file-name buffer-file-name (buffer-name)))

(defun listify-validate-variable (var &optional value)
  "Varidate VALUE in variable VAR matches custom-type
in symbol property.
If value matches `custom-type' in symbol property, return t.
Otherwise, value does not match nor custom-type is not set, return nil.
When VALUE is ommited or nil, current value of
VAR is validated."
  (let ((atype (custom-variable-type var)) (res nil))
    (if (null atype)
        (listify-message "Warning: Property `custom-type' for variable `%s' is not set." var)
      (unless value
        (setq value (symbol-value var)))
      (or
       (setq res (widget-apply (widget-convert atype) :match value))
       (listify-message "Warning: Value `%S' for variable `%s' does not match type `%s'."
                        value var atype)))
    res))

(defun listify-message-set-variables ()
  (let (cusvars ovars)
    (dolist (avar listify-init-set-variables)
      (if (custom-variable-p avar)
          (setq cusvars (append cusvars (list avar)))
        (setq ovars (append ovars (list avar)))))
    (listify-message
     (concat
      (when cusvars
        (format "Custom variables are set. - %s" (sort cusvars 'string<)))
      (when (and cusvars ovars)
        "\n")
      (when ovars
        (format "Variables are set. - %s" (sort ovars 'string<)))))))

(defun listify-set-variable (sym val now req comment)
  "Set a variable using `custom-set-variable'.

Arguments form:

    SYM VAL [ NOW[ REQ[ COMMENT]]]

Validation of `listify-validate-variable' is done
before setting. If failed, it cancels setting."
  (let (
        (oldval (purecopy (symbol-value sym)))
        (atype (custom-variable-type sym))
        (res nil)
        )
    (if (equal val oldval)
        (listify-message "Info: variable `%s': value `%s' is not changed." sym oldval)
      (when (null atype)
        (listify-message "Info: Type of variable `%s' is not defined." sym))
      (if (null (listify-validate-variable sym val))
            (listify-message "Warning: variable `%s': value `%s' is not validated. Type:" sym oldval atype)
        (custom-set-variables `(,sym ',val ,now ,req ,comment))
        (setq res sym)))
    res))

(defun listify-set-standard-value (sym)
  "Set current value of variable SYM to standard-value property."
  (let ((res nil))
    (if (null (boundp sym))
        (listify-message "Warning: In listify-set-standard-value, Variable `%s' is void." sym)
      (let (
            (val (purecopy (symbol-value sym)))
            (stdval (get sym 'standard-value))
            )
        (if stdval
            (listify-message "Variable %s: symbol property `standard-value' is already set. symbol property value: %s" sym stdval)
          (put sym 'standard-value
               ;;;; -- from: source code of defcustom in custom.el
               ;; (if lexical-binding
               ;;     ;; The STANDARD arg should be an expression that evaluates to
               ;;     ;; the standard value.  The use of `eval' for it is spread
               ;;     ;; over many different places and hence difficult to
               ;;     ;; eliminate, yet we want to make sure that the `standard'
               ;;     ;; expression is checked by the byte-compiler, and that
               ;;     ;; lexical-binding is obeyed, so quote the expression with
               ;;     ;; `lambda' rather than with `quote'.
               ;;     ``(funcall #',(lambda () "" ,val))
               ;;   `(',val)))
               `(',val))
          (listify-message "Property `standard-value' of variable `%s' is set as current value  by listify-set-standard-value. value: %s" sym (get sym 'standard-value))
          (setq res sym))))
    res))

(defun listify-set-standard-values (&rest syms)
  "Set current value of variable list SYMS to standard-value property."
  (mapcar (lambda(sym) (listify-set-standard-value sym)) syms))

(defun listify-set-variable-type (sym type)
  "Set TYPE of variable SYM. TYPE is set as SYM's variable property `custom-type'."
  (let (oldtype (res nil))
    (setq oldtype (get sym 'custom-type))
    (if oldtype
        (listify-message "Info: type of variable `%s' is already set. Type: %s" sym oldtype)
      (put sym 'custom-type type)
      (listify-message "Type of variable `%s' is set by listyfy-set-variable-type. Type: %s" sym (get sym 'custom-type))
      (setq res sym))
    res))

(defun listify-set-variable-types (&rest sym-types)
  "Set TYPE of variable SYM. TYPE is set as SYM's variable property `custom-type'.
Each SYM-TYPE has the form:

(SYMBOL TYPE)"
  (mapcar (lambda (asymtype) (listify-set-variable-type (car asymtype) (cadr asymtype))) sym-types))

(defun listify-set-alist (sym exp)
  "Add or update alist values of a custom variable.
Variable value must be a alist list type.
Arguments form:

    ALIST-NAME ((KEY1 VALUE1) (KEY2 VALUE2) ...)"
  (let ((newval nil))
    (if (null (boundp sym))
        (listify-message "Warning: In listify-set-alist, the 1st argument: `%s' is not a symbol." sym)
      (if (null (listp exp))
          (listify-message "Warning: In listify-set-alist, the 2nd argument is not list.\nArgument: %s" exp)
        (setq newval (copy-alist (symbol-value sym)))
        (dolist (akeyval exp)
          (if (null (consp akeyval))
              (listify-message "Warning: In listify-set-alist, element in the 2nd argument is not cons cell. Element: %s; argument: %s" akeyval exp)
            (setq newval (listify-add-or-update-alist newval (car akeyval) (cadr akeyval)))))))
    newval))

(defun listify-add-list (sym exp)
  "Add values of a custom variable.
Variable value must be a list type.
Arguments form:

    LIST-VARIABLE-NAME (VALUE1 VALUE2 ...)"
  (let ((newval nil))
    (if (null (boundp sym))
        (listify-message "Warning: In listify-add-list, the 1st argument: `%s' is not a symbol." sym)
      (if (null (listp exp))
          (listify-message "Warning: In listify-add-list, the 2nd argument is not list.\nArgument: %s" exp)
        (setq newval (nreverse (copy-sequence (symbol-value sym))))
        (dolist (aexp exp)
          (unless (member aexp newval)
            (push aexp newval)))))
    (nreverse newval)))

(defun listify-set (&rest args)
  "Set custom variable values specified in ARGS.
The arguments should each be a list of the form:

  (SYMBOL EXP [NOW [REQUEST [COMMENT]]])

The ARGS form is same to `custom-set-variables'.
Except EXP need no quote when EXP is SYMBOL,
and / or add each element when EXP is list,
update or add each element when EXP is association list (alist)."
  (let
      (asym exp anow areq acomm avar vars
            (oldval nil) (newval nil) afunc varstruct)
    (dolist (arg args)
      (setq
       asym (nth 0 arg) exp (nth 1 arg)
       anow (nth 2 arg) areq (nth 3 arg) acomm (nth 4 arg)
       oldval (purecopy (symbol-value asym)))
      (unless (custom-variable-p asym)
        (listify-set-standard-value asym))
      (if (or (null exp) (null (and (listp exp) (listp (cdr exp)))))
          (setq newval exp varstruct "Variable")
        (if (consp (car exp))
            (setq afunc 'listify-set-alist varstruct "Variable alist")
          (setq afunc 'listify-add-list varstruct "Variable list"))
        (setq newval (funcall afunc asym exp)))
      (setq avar
            (listify-set-variable
             asym newval anow areq
             (if acomm
                 acomm
               (format "Set in `%s' by `listify-set'." (listify-setting-file)))
             ))
      (when avar
        (listify-message "%s `%s': value `%s' is changed to `%s' by listify-set."
                         varstruct asym oldval (symbol-value asym))
        (unless (member avar listify-init-set-variables)
          (push avar listify-init-set-variables)))
      (push avar vars))
    vars))

(defun listify-update-cdrs (&rest args)
  "Update cdrs of alist variable SYM using CDRS-NEW-OLD alist.
The arguments should each be a list of the form:

    (SYMBOL ((CDR-NEW1 . CDR-OLD1) (CDR-NEW2 . CDR-OLD2) ... )
[NOW [REQUEST [COMMENT]]])"
  (let
      (asym cdrs anow areq acomm
            newval oldval avar vars)
    (dolist (arg args)
      (setq
       asym (nth 0 arg) cdrs (nth 1 arg)
       anow (nth 2 arg) areq (nth 3 arg) acomm (nth 4 arg)
       newval (purecopy (symbol-value asym))
       oldval (purecopy newval))
      (setq newval (listify-update-cdrs-alist newval cdrs))
      (setq avar
            (listify-set-variable
             asym newval anow areq
             (if acomm
                 acomm
               (format "Set in `%s' by `listify-update-cdrs'." (listify-setting-file)))))
      (when avar
        (listify-message "Alist variable `%s': value `%s' is changed to `%s' by listify-update-cdrs."
                         asym oldval (symbol-value asym))
        (unless (member avar listify-init-set-variables)
          (push avar listify-init-set-variables)))
      (push avar vars))
    vars))

(defun listify-variable-buffer-local (&rest vars)
  "Make VARIABLE become buffer-local by `make-variable-buffer-local'."
  (let ((res nil))
    (dolist (avar vars)
      (push (make-variable-buffer-local avar) res)
      (if (local-variable-if-set-p avar)
          (listify-message "Local variable `%s'." avar)
        (listify-message "Global variable `%s'." avar)))
    res))

(defun listify-defaliases (&rest sym-def)
  "Set SYMBOL’s function definition to DEFINITION in SYM-DEF.
Each SYM-DEF has the form:

(SYMBOL DEFINITION [DOCSTRING])."
  (let ((syms nil))
    (dolist (asymdef sym-def)
      (let ((asym (nth 0 asymdef)) (adef (nth 1 asymdef)) (adoc (nth 2 asymdef)) (res nil))
        (when (fboundp asym)
          (listify-message "Function `%s' is already defined as %s." asym (indirect-function asym)))
        (if (null (fboundp adef))
            (listify-message "Warning: In setting alias, symbol `%s' is not function." adef)
          (defalias asym adef adoc)
          (listify-message "Alias `%s' is defined as function `%s' by listify-defaliases." asym adef)
          (setq res asym))
        (push res syms)))
    syms))

(defun listify-require (feat)
  "Require feature FEAT, and the result is printed
into the `*Messages' buffer,
or the standard error stream in batch mode."
  (let ((res nil))
    (if (featurep feat)
        (listify-message "Info: Feature `%s' is already required." feat)
      (if (null (locate-library (symbol-name feat)))
          (listify-message "Warning: Feature `%s' is NOT found." feat)
        (if (null (require feat nil 1))
            (listify-message "Warning: require feature `%s' is failed" feat)
          (setq res feat))))
    res))

(defun listify-requires (&rest features)
  "Require FEATURES, and the result is printed
into the `*Messages' buffer,
or the standard error stream in batch mode."
  (let ((feats nil))
    (dolist (afeat features)
      (let ((res (listify-require afeat)))
        (when res
          (push res feats))))
    (setq feats (nreverse feats))
    (when feats
      (listify-message "Features are required by listify-requires. - %s" feats))
    feats))

(defun listify-requires-by-system (&rest sys-features)
  "If current system type or window system
got by VARIABLE is match to SYSTEM,
Require FEATURE by `listify-requires' in SYS-FEATURES.
Each SYS-FEATURES has the form:

    (SYSTEMTYPE SYSTEM FEATURE)"
  (let ((feats nil))
    (dolist (asysfeat sys-features)
      (let
          ((res nil) (asystype (nth 0 asysfeat))
           (asys (nth 1 asysfeat)) (afeat (nth 2 asysfeat)))
        (when (equal (eval asystype) asys)
          (listify-message "Feature `%s' of which system type `%s' matches `%s' is required by listify-requires-by-system." afeat asystype asys)
          (setq res (listify-require afeat))
          (when res
            (push res feats)))))
    feats))

(defun listify-autoloads-set (&rest func-file-doc)
  "Define autoload functions from FUNC-FILE-DOC.
Each FUNC-FILE-DOC has the form (FUNCTION FILE DOC).

If FUNCTION is void or FILE is not found,
warning message is printed into the `*Messages' buffer,
or the standard error stream in batch mode."
  (let (funcs)
    (dolist (ffd func-file-doc)
      (let ((afunc (car ffd)) (afile (cadr ffd)) (adoc (nth 2 ffd)))
        (if (not (locate-library afile))
            (listify-message "Warning: In setting autoload functions, library file `%s' autoloaded from `%s' is not found." afile afunc)
          (if (fboundp afunc)
              (listify-message "Info: In setting autoload functions, function `%s' is already defined." afunc)
            (condition-case aerr
                (push (autoload afunc afile adoc 1) funcs)
              (listify-message "Warning: In setting autoload functions, fails to set autoload `%s' from `%s'.\n%s" afunc afile aerr))))))
    (if (not funcs)
        (listify-message "Autoload functions is not defined.")
      (setq funcs (nreverse funcs))
      (listify-message "Autoload functions are defined by listify-autoloads-set. - %s" funcs)
      funcs)))

(defun listify-global-set-keys (&rest key-cmd)
  "Give global binding as KEY-CMD by global-set-key.
Each KEY-CMD has the form:

    (KEY CMD)

If CMD is void, warning message is printed into the `*Messages' buffer,
or the standard error stream in batch mode."
  (let ((res nil))
    (dolist (akeycmd key-cmd)
      (let ((akey (car akeycmd)) (acmd (cadr akeycmd)) (oldval nil))
        (setq oldval (keymap-lookup (current-global-map) akey))
        (if (not (or (equal '(keymap) acmd) (fboundp acmd)))
            (listify-message "Warning: In setting keybind, command `%s' is void." acmd)
          (if (or (equal oldval acmd) (and (listp oldval) (listp acmd) (equal (car oldval) (car acmd))))
              (listify-message "Key `%s' command `%s' is not changed." akey oldval)
            (keymap-global-set akey acmd)
            (listify-message "Key `%s' command `%s' is changed to `%s' by listify-global-set-keys"
                             akey oldval (keymap-lookup (current-global-map) akey))
            (push akey res)))))
    res))

(defun listify-global-unset-keys (&rest keys)
  (let ((res nil))
    (dolist (akey keys)
      (let ((oldval (keymap-lookup (current-global-map) akey)))
        (keymap-global-unset akey)
        (listify-message "Key `%s' command `%s' is unset by listify-global-unset-keys."
                         akey oldval)
        (push akey res)))
    res))

(defun listify-modemap-set-keys (&rest modemap)
  "Give KEY binding of MODEMAP as MAPKEYS after LIBRARY is loaded.
Each MODEMAP has the form:

    (MODEMAP LIBRARY HOOK ((KEY1 FUNCTION1) (KEY2 FUNCTION2) ... )).

If HOOK is not nil, the binding is via the HOOK.
If function in MAPKEYS is void, warning message is printed
into the `*Messages' buffer, or  the standard error stream in batch mode."
  (let (amap alib ahook keyfuncs amapname funcname funcdef)
    (dolist (amodemap modemap)
      (setq
       amap (nth 0 amodemap) alib (nth 1 amodemap)
       ahook (nth 2 amodemap) keyfuncs (nth 3 amodemap)
       amapname (symbol-name amap)
       funcname (read (concat "listify-" amapname "-keybind"))
       funcdef
       `(lambda ()
          "Add keymap settings. This function is defined in `listify-modemap-set-keys'.
To display Source code of this function, use `symbol-function'."
          (dolist (keyfunc ',keyfuncs)
            (let ((inhibit-message 1) (akey (car keyfunc)) (afunc (cadr keyfunc)) (oldval nil))
              (setq oldval (keymap-lookup ,amap akey))
              (if (not (fboundp afunc))
                  (listify-message ,(concat "In setting `" amapname "' keybind, function `%s' is void.") afunc)
                (keymap-set ,amap akey afunc)
                (listify-message
                 ,(concat "Key `%s' command `%s' in global map, `%s' in modemap `" amapname "', is changed to `%s' by " (symbol-name funcname) ".")
                 akey (keymap-lookup (current-global-map) akey)
                 oldval (keymap-lookup ,amap akey)))))))
      (fset funcname funcdef)
      (eval-after-load alib
        (if (null ahook)
            funcname
          `(add-hook ',ahook ',funcname))))))

(defun listify-set-minor-modes (&rest modeval)
  "Set MODES. MODE format is assumed as `(MINOR-MODE-FUNCTION ARG)'.
MINOR-MODE-FUNCTION presents minor mode.
Usually, `(MINOR-MODE-FUNCTION 1)' to enable the mode,
and `(MINOR-MODE-FUNCTION -1)' to disable the mode.

If FUNCTION in MODE is void, warning message is printed
into the `*Messages' buffer,
or the standard error stream in batch mode."
  (let (amode oldval newval)
    (dolist (amodeval modeval)
      (setq amode (car amodeval))
      (if (not (fboundp amode))
          (listify-message "Warning: In setting minor mode, function `%s' is void." amode)
        (setq oldval (eval amode))
        (eval amodeval)
        (setq newval (eval amode))
        (if (equal oldval newval)
            (listify-message "Info: minor mode - `%s': value `%s' is not changed." amode oldval)
          (listify-message "Minor mode - `%s': value `%s' is changed to `%s' by listify-set-minor-modes." amode oldval newval))))))

(defun listify-custom-initialize-hook (hook)
  "Initialize HOOK as a custom variable.
Set properties of standard-value and custom-type
of variable HOOK, if not set yet.
If HOOK is not defined, or
HOOK is already set as custom variable,
return nil. In other case, return HOOK."
  (let ((res nil))
    (if (null (boundp hook))
        (listify-message "Info: Hook `%s is not bound." hook)
      (unless (custom-variable-p hook)
        (setq res (listify-set-standard-value hook)))
      (unless (get hook 'custom-type)
        (setq res (listify-set-variable-type hook 'hook))))
    res))

(defun listify-custom-initialize-hooks (&rest hooks)
  (mapcar (lambda(ahook) (listify-custom-initialize-hook ahook)) hooks)
  )

(defun listify-set-hooks (&rest hook-funcs)
  "Set functions to each hook by list:

(HOOK1 (FUNC11 FUNC12 ...) (HOOK2 (FUNC21 FUNC22 ...) ...)

If FUNCTION or HOOK is void,
warning message is printed into the `*Messages' buffer,
or the standard error stream in batch mode."
  (let ((res nil))
    (dolist (ahookfuncs hook-funcs)
      (let ((ahook (car ahookfuncs)) (funcs (cadr ahookfuncs)))
        (listify-custom-initialize-hook ahook)
        (listify-set `(,ahook ,funcs))
        (push ahook res)))
    res))

(defun listify-setenv (&rest env-val)
  "Add system environment variables to values by list of ENV-VAL.
Each ENV-VAL form:

(ENVIRONMENT VALUE)"
  (let ((res nil))
    (dolist (aenvval env-val)
      (let ((env (car aenvval)) (newval (cadr aenvval)) oldval)
        (setq oldval (getenv env))
        (if (equal oldval newval)
            (listify-message "Info: System environment is not changed. Value: %s" oldval)
          (setenv env newval)
          (listify-message
           "System environment variable `%s': value `%s' is changed to `%s'."
           env oldval (getenv env))
          (push env res))))
    res))

(defun listify-eval-buffer ()
  "Execute the accessible portion of current buffer as Lisp code
by `eval-buffer' and message evaluted."
  (interactive)
  (eval-buffer)
  (message "%s is evaluted." (buffer-file-name)))

(provide 'listify)
;;; listify.el ends here
