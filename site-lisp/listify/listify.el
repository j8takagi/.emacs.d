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
  "Emacs start-up utility."
  :group 'initialization)

(defvar listify-init-set-variables nil
  "VARIABLES set in listify functions, mainly
from initialization files."
)

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
  (message (listify-get-variable-comment var)))

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
    (message
     (concat
      (when cusvars
        (format "Custom variables are set. - %s" cusvars))
      (when (and cusvars ovars)
        "\n")
      (when ovars
        (format "Variables are set. - %s" ovars))))))

(defun listify-set-alist (sym exp now req comment)
  "Add or update alist values of a custom variable.
Variable value must be a alist list type.
Arguments form:

ALIST-NAME ((KEY1 VALUE1) (KEY2 VALUE2) ...)[ NOW[ REQUEST[ COMMENT]]]."
  (let ((newval nil) (oldval nil))
    (unless (boundp sym)
      (error (format "In listify-set-alist, the 1st argument: `%s' is not a symbol." sym)))
    (unless (listp exp)
      (error (format "In listify-set-alist, the 2nd argument is not list.\nArgument: %s" exp)))
    (setq newval (symbol-value sym) oldval newval)
    (dolist (akeyval exp)
      (unless (consp akeyval)
        (error (format "In listify-set-alist, element in the 2nd argument is not cons cell.\nElement: %s; argument: %s" akeyval exp)))
      (setq newval (listify-add-or-update-alist newval (car akeyval) (cadr akeyval))))
    (if (null (listify-validate-custom-variable-type sym newval))
        (message "Warning: variable `%s' -- type is mismatch.\nType: %s\nValue: %s"
                 sym (custom-variable-type sym) newval)
      (if (equal newval oldval)
          (message "Info: alist variable `%s': value `%s' is not changed.\n" sym oldval)
        (custom-set-variables `(,sym ',newval ,now ,req ,comment))
        (message "Alist variable `%s': value `%s' is changed to `%s'."
                 sym oldval (symbol-value sym))))
    sym))

(defun listify-add-list (sym exp now req comment)
  "Add values of a custom variable.
Variable value must be a list type.
Arguments form:
LIST-VARIABLE-NAME (VALUE1 VALUE2 ...)[ NOW[ REQUEST[ COMMENT]]]."
  (let ((newval nil) (oldval nil))
    (unless (boundp sym)
      (error (format "In listify-add-list, the 1st argument: `%s' is not a symbol." sym)))
    (unless (listp exp)
      (error (format "In listify-add-list, the 2nd argument is not list.\nArgument: %s" exp)))
    (setq newval (symbol-value sym) oldval newval)
    (dolist (aexp exp)
      (unless (member aexp newval)
        (push aexp newval)))
    (if (null (listify-validate-custom-variable-type sym newval))
        (message "Warning: variable `%s' -- type is mismatch.\nType: %s\nValue: %s"
                 sym (custom-variable-type sym) newval)
      (if (equal newval oldval)
          (message "Info: list variable `%s': value `%s' is not changed.\n" sym oldval)
        (custom-set-variables
         `(,sym ',newval ,now ,req ,comment))
        (message "List variable `%s': value `%s' is changed to `%s'."
                 sym oldval (symbol-value sym))))
    sym))

(defun listify-set-atom (sym exp now req comment)
  "Set custom variable values specified in ARGS.
ARGS has form: (SYMBOL EXP [NOW [REQUEST [COMMENT]]]).
The ARGS form is same to `custom-set-variables'."
  (let ((newval nil) (oldval nil))
    (setq oldval (symbol-value sym))
    (if (null (listify-validate-custom-variable-type sym newval))
        (message "Warning: variable `%s' -- type is mismatch.\nType: %s\nValue: %s"
                 sym (custom-variable-type sym) newval)
      (if (equal exp oldval)
          (message "Variable `%s': value `%s' is not changed." sym oldval)
        (custom-set-variables `(,sym ',exp ,now ,req ,comment))
        (message "Variable `%s': value `%s' is changed to `%s'." sym oldval (eval sym))))
    sym))

(defun listify-set (&rest args)
  "Set custom variable values specified in ARGS.
The arguments should each be a list of the form:

  (SYMBOL EXP [NOW [REQUEST [COMMENT]]])

The ARGS form is same to `custom-set-variables'.
Except EXP need no quote when EXP is SYMBOL,
and/or add each element when EXP is list,
update or add each element when EXP is association list (alist)."
  (let (asym exp anow areq acomm vars)
    (dolist (arg args)
      (setq
       asym (nth 0 arg) exp (nth 1 arg) anow (nth 2 arg) areq (nth 3 arg)
       acomm (listify-create-variable-comment asym (nth 4 arg)))
      (setq vars
            (push
             (funcall
              (if (and exp (listp exp) (listp (cdr exp)))
                  (if (consp (car exp))
                      'listify-set-alist
                    'listify-add-list)
                'listify-set-atom)
              asym exp anow areq acomm)
             vars)))
    (listify-message-variables vars)
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
        (message "Warning: variable `%s' -- type is mismatch.\nType: %s\nValue: %s"
                 sym (custom-variable-type sym) newval)
      (if (equal newval oldval)
          (message "Alist variable `%s': value `%s' is not changed." sym oldval)
        (custom-set-variables `(,sym ',newval nil nil ,(listify-create-variable-comment sym "set by update-cdrs-variable")))
        (message "Variable `auto-mode-alist': value `%s' is changed to `%s'."
                 oldval (symbol-value sym))))))

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

;; Emacs開始にかかった時間をメッセージに表示
(defun message-startup-time ()
  "Message Duration of the Emacs initialization time."
  (message "Duration of the Emacs initialization - %s" (emacs-init-time)))

(defvar system-name-simple
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
          (message "Info: Feature `%s' is already required." afeat)
        (if (not (locate-library (symbol-name afeat)))
            (message "Warning: Feature `%s' is NOT found." afeat)
          (if (null (require afeat nil 1))
              (message "Warning: Fails to require feature - %s" afeat)
            (setq feats (append feats (list afeat)))))))
    (message "Features are required - %s" feats)))

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

(defun listify-global-set-keys (&rest key-func)
  "Give global binding as KEY-FUNC by global-set-key.
Each KEY-FUNC has the form:

    (KEY FUNCTION)

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

(defun listify-set-hook (hook func)
  "Add a function to hook by list of (HOOK FUNCTION).
If FUNCTION or HOOK is void,
warning message is printed into the `*Messages' buffer,
or the standard error stream in batch mode."
  (cond
   ((not (boundp hook))
    (message "Warning: In setting hook, hook `%s' is void." hook))
   ((not (fboundp func))
    (message "Warning: In setting hook, function `%s' is void." func))
   (t
    (if (member func (eval hook))
        (message "Hook - `%s': function `%s' is already a member." hook func)
      (add-hook hook func)))))

(defun listify-set-hooks (&rest hook-funcs)
  "Add a function to hook by list:

(HOOK1 (FUNC11 FUNC12 ...) (HOOK2 (FUNC21 FUNC22 ...) ...)

If FUNCTION or HOOK is void,
warning message is printed into the `*Messages' buffer,
or the standard error stream in batch mode."
  (dolist (ahookfuncs hook-funcs)
    (let ((ahook (car ahookfuncs)) (funcs (cadr ahookfuncs)) oldval newval)
      (dolist (afunc funcs)
        (setq oldval (eval ahook))
        (listify-set-hook ahook afunc))
      (if (equal oldval (setq newval (eval ahook)))
          (message "Hook `%s' is not changed." ahook)
        (message "Hook `%s': value `%s' is changed to `%s'." ahook oldval newval)))))

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
