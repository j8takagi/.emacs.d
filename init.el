;;;-*-Emacs-Lisp-*-
;; load-pathを追加
(setq load-path (append (list "~/.emacs.d/") load-path))

;; パッケージを使う（Emacs24）
(require 'package)

; Add package-archives
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; ついでにmarmaladeも追加

; Initialize
(package-initialize)

; melpa.el
(require 'melpa)

;; 日本語環境
(set-language-environment 'Japanese)

;; OSごとの設定
(when (window-system 'ns)
  (load "init-mac"))
(when (window-system 'x)
  (load "init-x"))

;; 起動時の画面を表示しない
(setq inhibit-startup-message t)

;; メニューバーを表示しない
(menu-bar-mode nil)

;; カーソルは点滅しない
(blink-cursor-mode 0)

;; 現在行に色を付ける
(global-hl-line-mode 1)

;; GCを減らす
;(setq gc-cons-threshold (* 100 gc-cons-threshold))

;; 履歴の数を増やす
;(setq history-length 500)

;; 重複する履歴は削除
(setq history-delete-duplicates 1)

;; エラー時は画面をフラッシュ
(setq visible-bell t)

;; メジャーモードに合わせた色を付ける font-lock-mode
(global-font-lock-mode 1)

;; ダイアログボックスは使わない
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;; タブの幅は、4
(setq-default tab-width 4)

;; タブをスペースに展開
(setq-default indent-tabs-mode nil)

;; タブ、全角スペース、行末の空白を表示する
(defface my-navy-box
  '((t (:box "navy"))) nil :group 'font-lock-highlighting-faces)
(defface my-orange-box
  '((t (:box "orange"))) nil :group 'font-lock-highlighting-faces)
(defface my-navy-underline
  '((t (:foreground "navy" :underline t))) nil :group 'font-lock-highlighting-faces)

(defvar my-navy-box 'my-navy-box)
(defvar my-orange-box 'my-orange-box)
(defvar my-navy-underline 'my-navy-underline)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("　" 0 my-orange-box append)
     ("\t" 0 my-navy-box append)
     ("[ ]+$" 0 my-navy-underline append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)

(ad-activate 'font-lock-mode)

(add-hook 'find-file-hooks
          '(lambda ()
             (if font-lock-mode nil
               (font-lock-mode t))) t)

;; 再帰的なミニバッファ
(setq enable-recursive-minibuffers t)

;; ファイル末尾で、end of bufferエラーなしで改行
(setq next-line-add-newlines nil)

;; 行番号を表示
(line-number-mode t)

;; narrow-to-regionを可能にする
(put 'narrow-to-region 'disabled nil)

;; erase-bufferを可能にする
(put 'erase-buffer 'disabled nil)

;; eval-expressionを可能にする
(put 'eval-expression 'disabled nil)

;; upcase-region, downcase-regionを可能にする
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; set-goal-columnを可能にする
(put 'set-goal-column 'disabled nil)

;; truncate
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; kill-lineのとき、改行も含めて切り取り
(setq kill-whole-line t)

;; 置換時に大文字小文字を区別しない
(setq case-replace nil)

;; リージョンをハイライト
(transient-mark-mode 1)

;; 括弧の対応を表示
(show-paren-mode 1)

;; 画面最下（上）部で下（上）向きにスクロールするとき、1行ずつスクロールさせる
(setq scroll-conservatively 1)

;; 行番号を表示
(column-number-mode 1)

;; バックアップファイルは、~/backupに格納
(setq backupdir "~/backup")
(when (file-exists-p backupdir)
  (setq backup-directory-alist
        (cons (cons "\\.*$" (expand-file-name backupdir))
              backup-directory-alist)))
(setq make-backup-files t)

;; sorter
(load "sorter")

;; uniq
(load "uniq")

;; 圧縮されたファイルを直接編集する
(auto-compression-mode)

;; URLをC-xC-fで開く
(ffap-bindings)

;; kill-lineのとき、改行も含めて切り取り
(setq kill-whole-line t)

;; 同一ファイル名のバッファ名を分かりやすく: uniquify
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; emacs lispファイル保存時に、バイトコンパイルする
;;    http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/junk_elisp.html#bytecompile
(add-hook 'after-save-hook
          (function
           (lambda ()
             (if (eq major-mode 'emacs-lisp-mode)
                 (save-excursion
                   (byte-compile-file buffer-file-name))))))

;; \M-?でヘルプ
(global-set-key "\M-?" 'help)

;; View mode
(global-set-key "\C-c\C-v" 'view-mode)

;; C-xKで、ウインドウとバッファを削除
(defun my-kill-buffer-window ()
  (interactive)
  (if (not (one-window-p))
      (progn
        (kill-buffer nil)
        (delete-window))
    (message "one-window-p")))
(global-set-key "\C-xK" 'my-kill-buffer-window)

;; C-x4Kで、隣のバッファを削除
(defun my-kill-next-buffer ()
  (interactive)
  (if (not (one-window-p))
        (kill-buffer (window-buffer(next-window)))
    (message "one-window-p")))
(global-set-key "\C-x4k" 'my-kill-next-buffer)

;; C-x4Kで、隣のバッファとウィンドウを削除
(defun my-kill-next-buffer-window ()
  (interactive)
  (if (not (one-window-p))
      (progn
        (kill-buffer (window-buffer(next-window)))
        (delete-window (next-window)))
    (message "one-window-p")))
(global-set-key "\C-x4K" 'my-kill-next-buffer-window)

;; M-pでマクロ
(global-set-key "\M-p" 'call-last-kbd-macro)
(global-set-key "\C-xp" 'call-last-kbd-macro)

;; auto indent
(define-key global-map "\C-m" 'newline-and-indent)
(setq indent-line-function 'indent-relative-maybe)

;; M-g: goto-line
(global-set-key "\M-g" 'goto-line)

;;;paragraph
(global-set-key "\M-[" 'backward-paragraph)
(global-set-key "\M-]" 'forward-paragraph)

;; \C,で1行上へスクロール
(defun scroll-up-one-line ()
  (interactive)
  (scroll-up 1))
(global-set-key '[?\C-,] 'scroll-up-one-line)

;; \C.で1行下へスクロール
(defun scroll-down-one-line ()
  (interactive)
  (scroll-down 1))
(global-set-key [?\C-.] 'scroll-down-one-line)

;; lisp-interaction-mode で M-[space] と M-[TAB] を交換する
(define-key lisp-interaction-mode-map "\e " 'lisp-complete-symbol)
(define-key lisp-interaction-mode-map "\e\t" 'dabbrev-expand)

;; \C-x\C-eでelectric-buffer-list
(global-set-key "\C-x\C-e" 'electric-buffer-list)

;; 日本語を数える
(defun count-japanese ()
  (interactive)
  (message "日本語の文字数: %d字" (how-many "\\cj" (point-min) (point-max))))

;; shell-command のコマンド入力に補完が効くようにする
(require 'shell-command)
(shell-command-completion-mode)

(setq shell-mode-hook
 '(lambda ()
    (setq shell-prompt-pattern "[~/][~/A-Za-z0-9_^$!#%&{}`'.,:()-]* \\[[0-9:]+\\] *$ ")
    (setq tab-width 4)))

(defadvice shell (after rename-shell ())
  "shellのバッファ名を変更する"
  (let ((num 0) (bufname "*shell*"))
    (save-excursion
      (while (get-buffer bufname)
        (progn
          (setq bufname (concat "*shell<" (number-to-string num) ">*"))
          (setq num (+ num 1)))))
    (rename-buffer bufname)))
(ad-activate 'shell)

(load "dired-x")

(add-hook 'dired-mode-hook
      (lambda ()
        (defun dired-open-file ()
          "In dired, `open' the file or directory named on this line."
          (interactive)
          (let ((file-name (file-name-sans-versions (dired-get-filename) t)))
        (if (file-exists-p file-name)
            (start-process "dired-open" nil "open" file-name)
          (if (file-symlink-p file-name)
              (error "File is a symlink to a nonexistent target")
            (error "File no longer exists; type `g' to update Dired buffer")))))
        (define-key dired-mode-map "r" 'dired-open-file)))

(require 'wdired)

(define-key dired-mode-map "\C-cw" 'wdired-change-to-wdired-mode)

(put 'dired-find-alternate-file 'disabled nil)

;; GNU GLOBAL
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-select-buffer-single t)
(global-set-key [?\C-\M-.] 'gtags-find-rtag)
(global-set-key [?\M-,] 'gtags-find-tag-from-here)
(global-set-key [?\C-\M-,] 'gtags-pop-stack)
(global-set-key "\C-c\C-f" 'gtags-find-file)
(global-set-key "\C-c\C-s" 'gtags-find-symbol)
(global-set-key "\C-c\C-p" 'gtags-find-pattern)

;;; CC-Mode
(require 'cc-mode)

(setq c-default-style "k&r")

(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; (c-toggle-hungry-state 1)
             (setq c-basic-offset 4)
             (setq indent-tab-mode nil)
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))

(defun set-compile-command-for-c ()
  (interactive)
  (let* ((filename (file-name-nondirectory buffer-file-name))
         (index (string-match "\\.c$" filename))
         (filename-no-suffix (substring filename 0 index)))
    (cond
     ;; make exists: "make -k"
     ((or (file-exists-p "Makefile")
          (file-exists-p "makefile"))
      (setq compile-command "make -k"))
     ;; header file exists: make object file
     ((file-exists-p (concat filename-no-suffix ".h"))
      (setq compile-command
            (concat "gcc -g -c " filename)))
     ;; others
     (t
      (setq compile-command
            (concat "gcc -o "
                    filename-no-suffix " " filename))))))

;; C-c c で compile コマンドを呼び出す
(global-set-key "\C-cc" 'compile)

;; bison-mode
(autoload 'bison-mode "bison-mode")

(add-hook 'bison-mode-hook
          '(lambda ()
             (setq bison-decl-token-column 0)
             (setq bison-rule-enumeration-column 8)))

;; *.y *.yy ファイルを 自動的に bison-mode にする
(setq auto-mode-alist (cons '("\\.yy?$" . bison-mode) auto-mode-alist))

(autoload 'flex-mode "flex-mode")

;; *.l *.ll ファイルを 自動的に flex-mode にする
(setq auto-mode-alist (cons '("\\.ll?$$" . flex-mode) auto-mode-alist))

;; Mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(eval-after-load "mew" '(require 'mew-browse))

;; mewメッセージファイルの開き方
;; Spotlightから.mewファイルを開けるようにする
(defun mew-open-mesg ()
  (interactive)
  (let ((mew-auto-get nil)
        (fld (mew-path-to-folder
              (directory-file-name (file-name-directory (buffer-file-name)))))
        (mes (file-name-sans-extension
              (file-name-nondirectory (buffer-file-name)))))
    (mew)
    (mew-summary-visit-folder fld)
    (mew-summary-move-and-display mes)))
(setq auto-mode-alist (cons '("\\.mew$" . mew-open-mesg) auto-mode-alist))

;; Optional setup (Read Mail menu for Emacs 21):
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))

;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))

(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

;; navi-2ch
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
(setq navi2ch-list-bbstable-url "http://menu.2ch.net/bbsmenu.html")

;; image
(auto-image-file-mode)

;; w3m
(require 'w3m-load)
(setq w3m-default-display-inline-images t)
(setq mew-use-w3m-minor-mode 1)

;; NXML
(autoload 'nxml-mode "nxml-mode" "major mode for editing XML" t)
(setq magic-mode-alist '(("<\\?xml " . nxml-mode)))
(setq auto-mode-alist (cons '("\\.xml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.svg$" . nxml-mode) auto-mode-alist))

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq nxml-child-indent 0)
            (setq indent-tabs-mode nil)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nxml-comment-content-face ((t (:foreground "yellow4"))))
 '(nxml-comment-delimiter-face ((t (:foreground "yellow4"))))
 '(nxml-delimited-data-face ((t (:foreground "lime green"))))
 '(nxml-delimiter-face ((t (:foreground "gray"))))
 '(nxml-element-local-name-face ((t (:inherit nxml-name-face :foreground "medium turquoise"))))
 '(nxml-name-face ((t (:foreground "rosy brown"))))
 '(nxml-tag-slash-face ((t (:inherit nxml-name-face :foreground "gray")))))

;; css-mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)

;; YaTeX
(setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "eplatex")

;; ChangeLog
(setq user-full-name "高木和人")
(setq user-mail-address "j8takagi@nifty.com")
(setq change-log-default-name "~/ChangeLog")

;; pukiwiki-mode
(load-library "pukiwiki-mode")

(setq pukiwiki-site-list
       '(("kanka" "http://plusone.ath.cx/pukiwiki/index.php" nil euc-jp)
         ("bookshelf" "http://www.bookshelf.jp/pukiwiki/pukiwiki" nil euc-jp)
         ("macemacs" "http://macemacsjp.sourceforge.jp/index.php" nil euc-jp)
         ("pukiwiki" "http://pukiwiki.org/index.php" nil utf-8)
         ))

;; iswitchb
(iswitchb-mode 1)

(setq read-buffer-function 'iswitchb-read-buffer)

(setq iswitchb-regexp t)

(setq iswitchb-prompt-newbuffer nil)

;; recentf
(require 'recentf-ext)

(global-set-key "\C-xm" 'man)

(require 'pdf-preview)

;; lcomp
;; 補完ウィンドウを補完完了時に消す
(require 'lcomp)
(lcomp-install)

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; browse-yank
(load "browse-yank")
(global-set-key "\M-y" 'browse-yank)

;; CASLII
(setq auto-mode-alist (cons '("\\.casl?$" . asm-mode) auto-mode-alist))

;; graphviz mode
(load "graphviz-dot-mode.el")

;; magit-mode
(require 'magit)

(setq auto-mode-alist (cons '("[Mm]akefile". makefile-gmake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.mk$". makefile-gmake-mode) auto-mode-alist))

;; ediff の操作用小ウィンドウを新規 frame にしない
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; GMP
(eval-after-load "info-look"
  '(let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
     (setcar (nthcdr 3 mode-value)
             (cons '("(gmp)Function Index" nil "^ -.* " "\\>")
                   (nth 3 mode-value)))))

; ESS
(require 'ess-site)
(setq auto-mode-alist
      (cons (cons "\\.[rR]$" 'R-mode) auto-mode-alist))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)

; CSV mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)


; Maxima
; http://emacswiki.org/emacs/MaximaMode
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

(setq auto-mode-alist
      (cons (cons "\\.mac$" 'maxima-mode) auto-mode-alist))
