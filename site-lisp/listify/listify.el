;;; listify.el --- Listify Emacs initialization files.

;; Copyright (C) 2017 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: init

;;; Commentary:
;;; Code:
(require 'cus-edit)

(defun update-or-add-alist (alist-var key value)
  "If KEY in ALIST, update VALUE of the KEY.
Unless, cons cell (KEY . VALUE) is added."
  (interactive)
  (let (acell (alst (symbol-value alist-var)))
   (if (setq acell (assoc key alst))
       (unless (equal (cdr acell) value)
         (setcdr acell value))
     (set alist-var (push (cons key value) alst)))
   alst))

(defun overwrite-values-alist (alist-var value-new-old-alist)
  "Overwrite ALIST-VAR alist by VALUE-NEW-OLD alist.
Each VALUE-NEW-OLD-ALIST has the form (VALUE-NEW . VALUE-OLD)."
  (let ((alst (symbol-value alist-var)) acell)
    (dolist (newold value-new-old-alist)
      (while (setq acell (rassoc (cdr newold) alst))
        (setcdr acell (car newold))))
    alst))

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
Each ALIST-ARGS has the form:

(ALIST-NAME ((KEY1 VALUE1) (KEY2 VALUE2) ...)[ NOW[ REQUEST[ COMMENT]]])."
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
Each LIST-VAL has the form

(LIST-NAME (VALUE1 VALUE2 ...)[ NOW[ REQUEST[ COMMENT]]])."
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
  (let (asym aexp anow areq acomm vars oldval)
    (dolist (arg args)
      (setq
       asym (nth 0 arg) aexp (nth 1 arg) anow (nth 2 arg) areq (nth 3 arg)
       acomm (listify-create-variable-comment asym (nth 4 arg)))
      (setq oldval (eval asym))
      (unless (equal aexp oldval)
        (if (and aexp (listp aexp) (listp (cdr aexp)))
            (setq vars
                  (append
                   vars
                   (funcall
                    (if (consp (car aexp))
                        'listify-set-alist
                      'listify-set-list)
                    (list asym aexp anow areq acomm))))
          (unless (listify-validate-custom-variable-type asym aexp)
            (message
             "%s: Variable type is mismatch.\nType: %s\nValue: %s"
             asym (custom-variable-type asym) aexp))
          (if (equal (symbol-value asym) aexp)
              (message "Variable `%s': value `%s' is not changed." asym oldval)
            (custom-set-variables
             (list
              asym
              (if (and aexp (not (eq aexp t)) (or (symbolp aexp) (listp aexp)))
                  `(quote ,aexp)
                aexp)
              anow
              areq
              acomm)))
          (message "Variable `%s': value `%s' is changed to `%s'." asym oldval (eval asym))
          (setq vars (append vars (list asym))))))
    (listify-message-variables vars)
    vars))

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

(defun listify-overwrite-auto-mode-alist (&rest mode-new-old)
  (let (anew aold alst)
    (dolist (newold mode-new-old)
      (cond
       ((not (fboundp (setq anew (car newold))))
        (message "Warning: In setting auto-mode-alist, mode `%s' overwritten from is void function." anew))
       ((not (fboundp (setq aold (cadr newold))))
        (message "Warning: In setting auto-mode-alist, mode `%s' overwritten to is void function." aold))
       (t
        (setq alst (push (cons anew aold) alst)))))
    (overwrite-values-alist 'auto-mode-alist alst)
    (message "auto-mode-alist: value `%s' is changed to `%s'." aold anew)))

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
        (message "hook - `%s': function `%s' is already member." hook func)
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
          (message "hook `%s' is not changed." ahook)
        (message "hook `%s': value `%s' is changed to `%s'." ahook oldval newval)))))

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
