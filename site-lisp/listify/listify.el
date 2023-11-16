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

(defun listify-update-cdrs (alist cdrs-new-old)
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

(defun listify-create-variable-comment (&optional add-comment)
  "Create variable comment of VAR by loading file or buffer file and ADD-COMMENT."
  (let (acomm afile)
    (when (setq afile (or load-file-name buffer-file-name (buffer-name)))
      (setq acomm (format "set in `%s'." afile)))
    (when add-comment
      (setq acomm (concat add-comment (when acomm " ") acomm)))
    acomm))

(defun listify-validate-custom-variable-type (custom-variable &optional value)
  "Varidate VALUES is match CUSTOM-VARIABLE to
custom-variable-type in symbol property.
If VALUE matches custom-variable-type in symbol properties list, t.
When VALUE is ommited or nil, current value of
CUSTOM-VARIABLE is validated."
  (let (atype)
    (when (null value)
      (setq value (symbol-value custom-variable)))
    (when (setq atype (custom-variable-type custom-variable))
      (widget-apply (widget-convert atype) :match value))))

(defun listify-message-set-variables()
  (let ((inhibit-message 1) cusvars ovars)
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

Validation of `listify-validate-custom-variable-type' is done
before setting. If fail, it cancels setting."
  (let ((oldval (purecopy (symbol-value sym))) (varstruct "Variable"))
    (when (listp oldval)
      (setq varstruct
            (if (consp (car oldval))
                "Alist variable"
              "List variable")))
    (if (equal val oldval)
        (listify-message "Info: alist variable `%s': value `%s' is not changed.\n" sym oldval)
      (if (null (listify-validate-custom-variable-type sym val))
          (listify-message "Warning: variable `%s' type mismatches value. -- Type: %s; Value: %s"
                   sym (custom-variable-type sym) val)
        (custom-set-variables `(,sym ',val ,now ,req ,comment))
        (listify-message "%s `%s': value `%s' is changed to `%s'." varstruct sym oldval (symbol-value sym))))
    sym))

(defun listify-set-variable-standard-value (sym)
  "Set current value of variable SYM to standard-value property."
  (if (null (boundp sym))
      (listify-message (format "`%s' is void." sym))
    (let (
          (val (purecopy (symbol-value sym)))
          (stdval (get sym 'standard-value))
          )
      (if stdval
          (listify-message "Variable %s: symbol property `standard-value' is already set. symbol property value: %s" sym stdval)
        (put sym 'standard-value `(',val))
        (listify-message "Variable %s: current value is set as property `standard-value'. value: %s" sym (get sym 'standard-value))))
    sym))

(defun listify-set-variables-standard-value (&rest syms)
  "Set current value of variable list in SYMS to standard-value property."
  (mapc
   (lambda(sym)
     (listify-set-variable-standard-value sym))
   syms))

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

(defun listify-update-cdrs-variable (sym cdrs-new-old)
  "Update cdrs of alist variable SYM using CDRS-NEW-OLD alist.
Arguments has the form:

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
  (let (amap alib ahook keyfuncs amapname funcdef)
    (dolist (amodemap modemap)
      (setq
       amap (car amodemap) alib (cadr amodemap)
       ahook (nth 2 amodemap) keyfuncs (nth 3 amodemap)
       amapname (symbol-name amap)
       funcdef
       `(lambda ()
          (dolist (keyfunc ',keyfuncs)
            (let ((akey (car keyfunc)) (afunc (cadr keyfunc)) (amapname nil) (oldval nil))
              (setq oldval (keymap-lookup ,amap akey))
              (if (not (fboundp afunc))
                  (listify-message
                   ,(concat "Warning: In setting `" amapname "' keybind, function `%s' is void.") afunc)
                (keymap-set ,amap akey afunc)
                (message "Key `%s' command `%s' in global map, `%s' in modemap `%s', is changed to `%s'"
                         akey
                         (keymap-lookup (current-global-map) akey) oldval amapname
                         (keymap-lookup ,amap akey)))))))
      (eval-after-load alib
        (if (null ahook)
            (eval funcdef)
          (let ((func-add-keybind (read (concat "listify-" amapname "-keybind"))))
            (fset func-add-keybind funcdef)
            `(add-hook ',ahook ',func-add-keybind)))))))

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
          (message "Warning: In setting minor mode, function `%s' is void." amode)
        (setq oldval (eval amode))
        (eval amodeval)
        (setq newval (eval amode))
        (if (equal oldval newval)
            (message "Info: minor mode - `%s': value `%s' is not changed." amode oldval)
          (message "Minor mode - `%s': value `%s' is changed to `%s'." amode oldval newval))))))

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
          (message "Hook `%s' is not defined." ahook)
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
  (message "%s is evaluted." (buffer-file-name)))

(provide 'listify)
;;; listify.el ends here
