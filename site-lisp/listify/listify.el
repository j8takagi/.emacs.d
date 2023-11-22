;;; listify.el --- Listify Emacs initialization files. -*- lexical-binding:t -*-
;; Copyright (C) 2017-2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: init

;;; Commentary:
;;; Code:
(require 'custom)
(require 'cus-edit)
(require 'wid-edit)

(defgroup listify nil
  "Emacs start-up utilities."
  :group 'initialization)

(defvar listify-init-set-variables nil
  "Variables set in listify functions,
mainly from initialization files."
)

(defvar listify-inhibit-message nil
  "Non-nil means calls to `listify-message' are
not display and not logged to th *Messages* buffer."
)

(defun listify-message (format-string &rest args)
  "Display a message using `message' if `listify-inhibit-message' is nil."
  (when noninteractive
    (setq listify-inhibit-message t))
  (unless listify-inhibit-message
    (apply #'message format-string args)))

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
    (unless (boundp sym)
      (error (format "In listify-set-alist, the 1st argument: `%s' is not a symbol." sym)))
    (unless (listp exp)
      (error (format "In listify-set-alist, the 2nd argument is not list.\nArgument: %s" exp)))
    (setq newval (copy-alist (symbol-value sym)))
    (dolist (akeyval exp)
      (unless (consp akeyval)
        (error (format "In listify-set-alist, element in the 2nd argument is not cons cell.\nElement: %s; argument: %s" akeyval exp)))
      (setq newval (listify-add-or-update-alist newval (car akeyval) (cadr akeyval))))
    newval))

(defun listify-add-list (sym exp)
  "Add values of a custom variable.
Variable value must be a list type.
Arguments form:

    LIST-VARIABLE-NAME (VALUE1 VALUE2 ...)"
  (let ((newval nil))
    (unless (boundp sym)
      (error (format "In listify-add-list, the 1st argument: `%s' is not a symbol." sym)))
    (unless (listp exp)
      (error (format "In listify-add-list, the 2nd argument is not list.\nArgument: %s" exp)))
    (setq newval (copy-sequence (symbol-value sym)))
    (dolist (aexp exp)
      (unless (member aexp newval)
        (push aexp newval)))
    newval))

(defun listify-set (&rest args)
  "Set custom variable values specified in ARGS.
The arguments should each be a list of the form:

  (SYMBOL EXP [NOW [REQUEST [COMMENT]]])

The ARGS form is same to `custom-set-variables'.
Except EXP need no quote when EXP is SYMBOL,
and / or add each element when EXP is list,
update or add each element when EXP is association list (alist)."
  (let (asym exp anow areq acomm vars (newval nil))
    (dolist (arg args)
      (setq
       asym (nth 0 arg) exp (nth 1 arg) anow (nth 2 arg) areq (nth 3 arg)
       acomm (listify-create-variable-comment (nth 4 arg)))
      (unless (custom-variable-p asym)
        (listify-set-variable-standard-value asym))
      (setq newval
              (if (and exp (listp exp) (listp (cdr exp)))
                  (funcall
                   (if (consp (car exp))
                       'listify-set-alist
                    'listify-add-list)
                   asym exp)
                exp))
      (setq vars
            (push (listify-set-variable asym newval anow areq acomm) vars)))
    (setq listify-init-set-variables (append listify-init-set-variables vars))
    vars))

(defun listify-update-cdrs (&rest args)
  "Update cdrs of alist variable SYM using CDRS-NEW-OLD alist.
The arguments should each be a list of the form:

    SYM ((CDR-NEW1 . CDR-OLD1) (CDR-NEW2 . CDR-OLD2)) ..."
  (let ((newval nil) (oldval nil))
    (setq
     oldval (copy-alist (symbol-value sym))
     newval (listify-update-cdrs auto-mode-alist cdrs-new-old))
    (if (null (listify-validate-custom-variable-type sym newval))
        (listify-message "Warning: variable `%s' -- type is mismatch.\nType: %s\nValue: %s"
                 sym (custom-variable-type sym) newval)
      (if (equal newval oldval)
          (listify-message "Alist variable `%s': value `%s' is not changed." sym oldval)
        (custom-set-variables `(,sym ',newval nil nil ,(listify-create-variable-comment "By update-cdrs-variable,")))
        (listify-message "Variable `auto-mode-alist': value `%s' is changed to `%s'."
                 oldval (symbol-value sym))))
    sym))

(defun listify-defaliases (&rest sym-def)
  "Set SYMBOL’s function definition to DEFINITION in SYM-DEF.
Each SYM-DEF has the form (SYMBOL DEFINITION &optional DOCSTRING)."
  (let (asym adef)
    (dolist (asymdef sym-def)
     (when (fboundp (setq asym (car asymdef)))
       (listify-message "Info: Function `%s' is already defined as %s." asym (indirect-function asym)))
     (if (not (fboundp (setq adef (cadr asymdef))))
         (listify-message "Warning: In setting alias, symbol `%s' is not function." adef)
       (defalias asym adef (nth 3 asymdef))
       (listify-message "`%s' is defined as alias of `%s'." asym adef)))))

(defvar listify-system-name-simple
  (replace-regexp-in-string "\\..*\\'" "" (system-name))
  "The simple host name of the machine Emacs is running on,
which is without domain information.")

(defun listify-requires (&rest feature)
  "Require FEATURE, and the result is printed
into the `*Messages' buffer,
or the standard error stream in batch mode."
  (let (feats)
    (dolist (afeat feature)
      (if (featurep afeat)
          (listify-message "Info: Feature `%s' is already required." afeat)
        (if (not (locate-library (symbol-name afeat)))
            (listify-message "Warning: Feature `%s' is NOT found." afeat)
          (if (null (require afeat nil 1))
              (listify-message "Warning: Fails to require feature - %s" afeat)
            (setq feats (append feats (list afeat)))))))
    (listify-message "Features are required - %s" feats)))

(defun listify-requires-by-system (&rest sys-features)
  "If current system type or window system got by VARIABLE is match to SYSTEM,
Require FEATURE by `listify-requires' in SYS-FEATURES.

Each element of SYS-FEATURES has the form (VARIABLE SYSTEM FEATURE)."
  (dolist (asysfeat sys-features)
    (when (equal (eval (car asysfeat)) (cadr asysfeat))
      (listify-requires (nth 2 asysfeat)))))

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
              (error
               (listify-message "Warning: In setting autoload functions, fails to set autoload `%s' from `%s'.\n%s" afunc afile aerr)))))))
    (if (not funcs)
        (listify-message "Autoload functions is not defined.")
      (listify-message "Autoload functions are defined. - %s" (reverse funcs)))))

(defun listify-global-set-keys (&rest key-cmd)
  "Give global binding as KEY-CMD by global-set-key.
Each KEY-CMD has the form:

    (KEY CMD)

If CMD is void, warning message is printed into the `*Messages' buffer,
or the standard error stream in batch mode."
  (dolist (akeycmd key-cmd)
    (let ((akey (car akeycmd)) (acmd (cadr akeycmd)) (oldval nil))
      (setq oldval (keymap-lookup (current-global-map) akey))
      (if (not (fboundp acmd))
          (listify-message "Warning: In setting keybind, command `%s' is void." acmd)
        (if (equal oldval acmd)
            (listify-message "Key `%s' command `%s' is not changed." akey oldval)
          ;; (global-set-key (kbd akey) acmd)
          (keymap-global-set akey acmd)
          (listify-message "Key `%s' command `%s' is changed to `%s'"
                   akey oldval (keymap-lookup (current-global-map) akey))
        )))))

(defun listify-global-unset-keys (&rest keys)
  (dolist (akey keys)
    (keymap-global-unset akey)))

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
          (dolist (keyfunc ',keyfuncs)
            (let ((inhibit-message 1) (akey (car keyfunc)) (afunc (cadr keyfunc)) (oldval nil))
              (setq oldval (keymap-lookup ,amap akey))
              (if (not (fboundp afunc))
                  (listify-message
                   ,(concat "Warning: In setting `" amapname "' keybind, function `%s' is void.") afunc)
                (keymap-set ,amap akey afunc)
                (listify-message "Key `%s' command `%s' in global map, `%s' in modemap `%s', is changed to `%s'"
                         akey
                         (keymap-lookup (current-global-map) akey) oldval ,amapname
                         (keymap-lookup ,amap akey)))))))
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
          (listify-message "Minor mode - `%s': value `%s' is changed to `%s'." amode oldval newval))))))

(defun listify-custom-initialize-hooks (&rest hooks)
  (mapcar (lambda(ahook) (listify-custom-initialize-hook ahook)) hooks)
  )

(defun listify-custom-initialize-hook (hook)
  "Initialize HOOK as a custom variable.
Set properties of standard-value and custom-type
of variable HOOK, if not set yet.
If HOOK is not define, return nil.
In other case, return HOOK."
  (if (not (boundp hook))
      nil
    (unless (custom-variable-p hook)
      (listify-set-variable-standard-value hook))
    (unless (get hook 'custom-type)
      (put hook 'custom-type 'hook))
    hook))

(defun listify-set-hooks (&rest hook-funcs)
  "Set functions to each hook by list:

(HOOK1 (FUNC11 FUNC12 ...) (HOOK2 (FUNC21 FUNC22 ...) ...)

If FUNCTION or HOOK is void,
warning message is printed into the `*Messages' buffer,
or the standard error stream in batch mode."
  (dolist (ahookfuncs hook-funcs)
    (let ((ahook (car ahookfuncs)) (funcs (cadr ahookfuncs)))
      (if (null (listify-custom-initialize-hook ahook))
          (listify-message "Hook `%s' is not defined." ahook)
        (listify-set `(,ahook ,funcs))))))

(defun listify-setenv (&rest env-val)
  "Add system environment variables to values by list of ENV-VAL.
Each ENV-VAL form is (ENVIRONMENT VALUE)."
  (dolist (aenvval env-val)
    (setenv (car aenvval) (cadr aenvval))))

(defun listify-eval-buffer ()
  "Execute the accessible portion of current buffer as Lisp code
by `eval-buffer' and message evaluted."
  (interactive)
  (eval-buffer)
  (listify-message "%s is evaluted." (buffer-file-name)))

(provide 'listify)
;;; listify.el ends here
