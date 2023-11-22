;;; early-init.el -*- lexical-binding: t -*-
(mapc
 (lambda (var)
   (when (and (boundp var) (null (custom-variable-p var)))
     (put var 'standard-value `(',(eval var)))))
 '(
   auto-mode-alist
   default-directory
   disabled-command-function
   file-name-coding-system
   indent-line-function
   interpreter-mode-alist
   locale-coding-system
   magic-mode-alist
   ))

;; 変数のデータ型を設定
(mapc
 (lambda (vartype)
   (let ((avar (car vartype)) (atype (cadr vartype)))
     (when (and (boundp avar) (null (get avar 'custom-type)))
       (put avar 'custom-type `,atype))))
 '(
   (auto-mode-alist
    (repeat
     (choice
      (cons
       (regexp :tag "Regexp matching file name")
       (symbol :tag "Major mode function"))
      (list
       (regexp :tag "Regexp matching file name")
       (symbol :tag "Function")
       (sexp :tag "NON-NIL stands for anything that is not nil")))))
   (default-directory (directory :must-match t))
   (disabled-command-function (choice (function :value disabled-command-function) (const nil)))
   (file-name-coding-system (coding-system))
   (indent-line-function (symbol :tag "Function to indent the current line"))
   (interpreter-mode-alist
    (alist
     :key-type (regexp :tag "Regexp matches the entire name of the interpreter")
     :value-type (symbol :tag "Major mode")))
   (magic-mode-alist
    (alist
     :key-type
     (choice
      (regexp :tag "Regexp matches the text at the beginning of the buffer")
      (symbol :tag "Match function"))
     :value-type (symbol :tag "Major mode")))
   (default-frame-alist
    (repeat (cons :format "%v" (symbol :tag "Parameter") (sexp :tag "Value"))))
   ))

(mapc
 (lambda (var)
   (when (boundp var)
     (when (null (custom-variable-p var))
       (put var 'standard-value `(',(eval var))))
     (when (null (get var 'custom-type))
       (put var 'custom-type 'hook))))
 '(
   activate-mark-hook
   after-change-functions
   after-change-major-mode-hook
   after-delete-frame-functions
   after-init-hook
   after-insert-file-functions
   after-make-frame-functions
   after-save-hook
   after-setting-font-hook
   auto-save-hook
   before-change-functions
   before-hack-local-variables-hook
   before-init-hook
   before-make-frame-hook
   before-save-hook
   buffer-access-fontify-functions
   buffer-list-update-hook
   buffer-quit-function
   change-major-mode-after-body-hook
   change-major-mode-hook
   command-line-functions
   deactivate-mark-hook
   delayed-warnings-hook
   delete-frame-functions
   delete-terminal-functions
   echo-area-clear-hook
   emacs-startup-hook
   find-file-hook
   find-file-not-found-functions
   first-change-hook
   focus-in-hook
   focus-out-hook
   font-lock-extend-after-change-region-function
   font-lock-extend-region-functions
   font-lock-fontify-buffer-function
   font-lock-fontify-region-function
   font-lock-mark-block-function
   font-lock-syntactic-face-function
   font-lock-unfontify-buffer-function
   font-lock-unfontify-region-function
   fontification-functions
   frame-auto-hide-function
   hack-local-variables-hook
   kill-buffer-hook
   kill-buffer-query-functions
   kill-emacs-hook
   kill-emacs-query-functions
   menu-bar-update-hook
   minibuffer-exit-hook
   minibuffer-setup-hook
   mouse-leave-buffer-hook
   mouse-position-function
   pop-up-frame-function
   post-command-hook
   post-gc-hook
   post-self-insert-hook
   pre-command-hook
   pre-redisplay-functions
   prefix-command-echo-keystrokes-functions
   prefix-command-preserve-state-hook
   quit-window-hook
   resume-tty-functions
   split-window-preferred-function
   suspend-hook
   suspend-resume-hook
   suspend-tty-functions
   syntax-propertize-extend-region-functions
   syntax-propertize-function
   temp-buffer-setup-hook
   temp-buffer-show-function
   temp-buffer-show-hook
   tty-setup-hook
   window-configuration-change-hook
   window-scroll-functions
   window-setup-hook
   window-size-change-functions
   write-contents-functions
   write-file-functions
   write-region-annotate-functions
   write-region-post-annotation-function
 ))
