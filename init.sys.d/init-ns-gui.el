;; -*- mode: Emacs-Lisp; -*-
;; Mac OS X GUI用の設定

(message "Start of loading %s." load-file-name)

(require 'listify)

(listify-requires
 'fontset-set
 ;; 'mac-ime-cursor
 )

;; Mac OS Xのpath_helperでPATHを取得し、あらためてPATHとして設定
(let ((shell-file-name "/bin/bash"))
    (setenv "PATH"
            (shell-command-to-string
             "eval $(/usr/libexec/path_helper -s) && printf $PATH")))

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(setq exec-path nil)

(dolist
    (dir (split-string (getenv "PATH") "[:\n]"))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir t)))

;; 環境変数の設定
(listify-setenv
 '("LANG" "en_US.UTF-8")
 '("PAGER" "cat")
 '("MANPAGER" "cat")
 '("EDITOR" "emacsclient")
 '("VISUAL" "emacsclient")
 )

(listify-set
 `(default-frame-alist                  ; デフォルトフレーム
    (
     (width 180)
     (height 56)
     (top 23)
     (left 0)
     (font
      ,(fontset-set
        '(
          (ascii (font-spec :family "Menlo" :weight 'normal :slant 'normal :size 12))
          (unicode (font-spec :family "Hiragino Sans"))
          ) "my_default_mac"))
     ))
; '(face-font-rescale-alist (("Hiragino Sans" 1.167) ("YuGothic" 1.167)))
 '(ns-command-modifier meta)        ; commandキーをEmacsのMetaキーに
 '(ns-alternate-modifier none)
; '(mac-auto-ascii-mode t) ; ミニバッファへのカーソル移動時、日本語IMEを自動オフ
 )

(fontsets-set
 '((
    (ascii (font-spec :family "Menlo" :weight 'bold :slant 'normal :size 12))
    (unicode (font-spec :family "Hiragino Sans" :weight 'bold :slant 'normal))
    ) "my_bold")
 '((
    (ascii (font-spec :weight 'normal :slant 'italic))
    (unicode (font-spec :family "Hiragino Sans" :weight 'semibold :slant 'normal))
    ) "my_italic")
 '((
    (ascii (font-spec :family "Menlo" :weight 'bold :slant 'italic :size 12))
    (unicode (font-spec :family "Hiragino Sans" :weight 'bold :slant 'normal))
    ) "my_bold_italic")
 '((
    (unicode (font-spec :family "Hiragino Sans" :weight 'normal :slant 'normal :size 12))
    ) "my_gothic")
 '((
    (unicode (font-spec :family "Hiragino Mincho ProN" :weight 'normal :slant 'normal :size 12))
    ) "my_mincho")
 '((
    (unicode (font-spec :family "YuKyokasho" :weight 'normal :slant 'normal :size 12))
    ) "my_yu_kyokasho")
 '((
    (unicode (font-spec :family "YuMincho" :weight 'normal :slant 'normal :size 12))
    ) "my_yu_mincho")
 '((
    (ascii (font-spec :family "Menlo" :weight 'normal))
    (unicode (font-spec :family "YuGothic"))
    ) "my_yu_gothic")
 '((
    (unicode (font-spec :family "Toppan Bunkyu Gothic"))
    ) "my_bunkyu_gothic")
 '((
    (unicode (font-spec :family "Hiragino Maru Gothic ProN"))
    ) "my_maru_gothic")
 '((
    (unicode (font-spec :family "Klee"))
    ) "my_klee")
 '((
    (unicode (font-spec :family "Source Han Code JP"))
    ) "my_sourcehancode")
 '((
   (ascii (font-spec :family "Source Han Code JP" :weight 'normal :slant 'normal :size 12))
   (unicode (font-spec :family "Toppan Bunkyu Gothic" :weight 'normal :slant 'normal))
    ) "my_alternative")
 )

(set-face-fontset 'italic (fontset-set-alias2spec "my_italic"))

;; view-modeの設定
(with-eval-after-load 'set-view-mode
  (listify-set
   '(set-view-mode-read-write-directory-patterns
     (
      "~/Documents"
      "~/.emacs.d/elpa" "/tmp" "/var"
      ))))

(listify-requires 'dictionary_app)

;; (listify-set
;;  '(mac-selected-keyboard-input-source-change-hook mac-ime-cursor-change-color)
;;  '(focus-in-hook mac-ime-cursor-change-color)
;;  )

;;; ファイルをQuick Lookupで開く
(defun quicklookup (file)
  "Quick look a file in Mac OS."
  ;;; 指定したファイルが実在しない場合はエラー終了
  (unless (file-exists-p file)
    (error "%s: File does not exist." file))
  (let (cmdstr (procname "quicklookup"))
    (setq cmdstr
          (mapconcat 'identity
                     (list "qlmanage" "-p" (shell-quote-argument file))
                     " "))
    (message "%s at %s: %s" procname (format-time-string "%Y/%m/%d %H:%M:%S") cmdstr)
    (start-process-shell-command procname (messages-buffer) cmdstr)))

;; dired-modeでカーソル下のファイルやディレクトリーをQuick Lookupで開く
(defun dired-quicklookup ()
  "Open file mentioned on this line in Quick Lookup."
  (interactive)
  (quicklookup (dired-get-filename)))


;; Mac OS Xのキー設定
(listify-global-set-keys
 '("<M-f1>" other-frame)    ; Mac OS Xの他アプリと同様に、command + F1でアプリケーションの次のウィンドウを操作対象にする
 '("C-c C-M-d" dictionary_app)
 '("C-c C-d" dictionary_app-at-point)
 )

(listify-modemap-set-keys
 '(dired-mode-map "dired" nil
   (
    ("SPC" dired-quicklookup))))

(remove-hook 'first-change-hook 'ns-unselect-line)

(cd "~")

(provide 'init-ns-gui)
