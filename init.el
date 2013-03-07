;;;-*-Emacs-Lisp-*-
;; load-pathを追加
(add-to-list 'load-path "~/.emacs.d/")

;; site-lispディレクトリーを~/.emacs.d/site-lispに
(defvar site-lisp-dir (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path site-lisp-dir)
(let ((default-directory site-lisp-dir))
      (load (expand-file-name "subdirs")))

;; パッケージを使う（Emacs24）
(require 'package)

; パッケージアーカイブ
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

; パッケージ初期化
(package-initialize)

; melpa.el
(require 'melpa)

;; 日本語環境
(set-language-environment 'Japanese)

;; Windowシステムごとの設定
(if (eq window-system 'ns) (load "init-mac"))
(if (eq window-system 'x) (load "init-ubuntu-x"))

;; OSごとの設定
(if (eq system-type 'gnu/linux) (load "init-linux"))

;; 起動時の画面を表示しない
(setq inhibit-startup-message t)

;; メニューバーを表示しない
(menu-bar-mode nil)

;; カーソルは点滅しない
(blink-cursor-mode 0)

;; 履歴の数を増やす
(setq history-length 100)

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

;; whitespace
(require 'whitespace)

(setq whitespace-style '(face tabs spaces trailing))
(setq whitespace-space-regexp "\\(　\\)")
(setq whitespace-trailing-regexp "\\( +$\\)")

(set-face-attribute whitespace-tab nil :box "navy" :background "inherit")
(set-face-attribute whitespace-space nil :box "orange" :background "inherit")
(set-face-attribute whitespace-trailing nil
                    :foreground "navy" :background "inherit" :underline "navy")

;; whitespaceを無効にするメジャーモード
(defvar whitespace-disabled-major-mode-list)
(setq whitespace-disabled-major-mode-list
      '(mew-summary-mode completion-list-mode help-mode
        magit-mode tetris-mode))

;; メジャーモード設定後、whitespaceを有効にする
(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (unless (member major-mode whitespace-disabled-major-mode-list)
               (whitespace-mode 1))))

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

;; 画面最下部で下向き、画面最上部で上向きにスクロールするとき、
;; 1行ずつスクロール
(setq scroll-conservatively 1)

;; 行番号を表示
(column-number-mode 1)

;; バックアップファイルは、~/backupに格納
(setq backup-dir (expand-file-name "~/backup"))

(if (file-exists-p backup-dir)
    (progn
      (setq backup-directory-alist `(("." . ,backup-dir)))
      (setq make-backup-files t)
      (setq version-control t)
      (setq delete-old-versions t)))

;; 隣のバッファファイルを閉じる。ウィンドウはそのまま
(defun my-kill-next-buffer ()
  (interactive)
  (if (one-window-p)
      (message "one-window-p")
    (kill-buffer (window-buffer (next-window)))))

;; 現在のバッファファイルと隣のバッファファイルを閉じる。ウィンドウはそのまま
(defun my-kill-current-next-buffer ()
  (interactive)
  (my-kill-next-buffer-window)
  (kill-buffer (current-buffer)))

;; 隣のバッファファイルを閉じ、ウィンドウも閉じる
(defun my-kill-next-buffer-window ()
  (interactive)
  (if (one-window-p)
      (message "one-window-p")
    (kill-buffer (window-buffer (next-window)))
    (delete-window (next-window))))

;; インデント
(setq indent-line-function 'indent-relative-maybe)

;; 1行上へスクロール
(defun scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

;; 1行下へスクロール
(defun scroll-down-one-line ()
  (interactive)
  (scroll-down 1))

;; 日本語を数える
(defun count-japanese ()
  (interactive)
  (message "日本語の文字数: %d字" (how-many "\\cj" (point-min) (point-max))))

;; iswitchb
(iswitchb-mode 1)
(setq read-buffer-function 'iswitchb-read-buffer)
(setq iswitchb-regexp t)
(setq iswitchb-prompt-newbuffer nil)

;; テンポラリバッファを開く
(defun switch-to-temp-buffer ()
  "Create temporary buffer."
  (interactive)
  ;; バッファ名は現在の日時
  (switch-to-buffer
   (generate-new-buffer
    (concat "*" (replace-regexp-in-string " +" "_" (current-time-string)) "*"))
  (setq buffer-offer-save nil)))

;; uniq
(load "uniq")

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'ediff)
(declare-function ediff-vc-internal "ediff-vers" (REV1 REV2 &optional STARTUP-HOOKS))
(defun ediff-vc-latest-current ()
  "Run Ediff of buffer file by comparing the latest and current."
  (interactive)
  (let ((file) (state))
    (setq file (buffer-file-name))
    (unless file
      (error "buffer not visiting file"))
    (setq state (vc-state file))
    (if (member state '(up-to-date added))
        (message "%s: %s" file state)
      (ediff-load-version-control)
      (ediff-vc-internal "" ""))))

(require 'vc)
(defun find-file-revision (&optional file revision)
  "find-file FILE REVISION.
  Input FILE first, REVISION then.
  Or, input FILE as 'FILE.~REVISON~' and FILE and REVISION is specified."
  (interactive "P")
  (if (not (stringp file))
    (setq file (expand-file-name (read-file-name "Find version controled file: "))))
  ;; find-file FILE REVISION by 'FILE.~REVISION~'."
  (if (string-match "\\(.+\\)\\.~\\(.+\\)~$" file)
      (progn
        (setq revision (substring file (match-beginning 2) (match-end 2)))
        (setq file (substring file (match-beginning 1) (match-end 1)))))
  (unless (vc-backend file)
    (error (format "%s is not under version control." file)))
  (unless (stringp revision)
    (setq revision
          (vc-read-revision
           (format
            "Revision (default %s's working revision): " (file-name-nondirectory file))
           (list file))))
  (if (string= revision "")
      (setq revision nil))
  (set-buffer (find-file-noselect file))
  (switch-to-buffer (vc-find-revision file revision)))

;; 圧縮されたファイルを直接編集する
(auto-compression-mode)

;; URLをC-xC-fで開く
(ffap-bindings)

;; kill-lineのとき、改行も含めて切り取り
(setq kill-whole-line t)

;; lcomp
;; 補完ウィンドウを補完完了時に消す
(require 'lcomp)
(lcomp-install)

;; browse-yank
(load "browse-yank")

;; 同一ファイル名のバッファ名を分かりやすく: uniquify
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; magit-mode
(autoload 'magit-status "magit" nil t)

;; git
(require 'git)
(require 'git-blame)

(defun my-insert-filename (filename)
  (interactive "*fInsert file name: ")
  (insert filename))

(global-set-key "\C-c\C-c" 'comment-region)              ; コメントを付ける
(global-set-key "\C-xve" 'ediff-vc-latest-current)       ; 最新版と現在のファイルでEdiff
(global-set-key "\C-xvf" 'find-file-revision)            ; ファイル旧版を開く
(global-set-key "\C-c\C-u" 'uncomment-region)            ; コメントを外す
(global-set-key "\C-ct" 'switch-to-temp-buffer)          ; テンポラリバッファを開く
(global-set-key "\C-c\C-v" 'view-mode)                   ; View mode
(global-set-key "\C-cc" 'compile)                        ; make
(global-set-key "\C-cg" 'magit-status)                   ; magit
(global-set-key "\C-cww" 'whitespace-mode)               ; whitespace-mode
(global-set-key "\C-cwt" 'whitespace-toggle-options)     ; whitespace-toggle-options
(global-set-key "\C-m" 'newline-and-indent)              ; インデント
(global-set-key "\C-x4K" 'my-kill-next-buffer-window)    ; 隣のバッファとウィンドウを削除
(global-set-key "\C-x4k" 'my-kill-next-buffer)           ; 隣のバッファを削除
(global-set-key "\C-xK" 'my-kill-current-next-buffer)    ; 現在のバッファと隣のバッファを削除
(global-set-key "\C-x\C-e" 'electric-buffer-list)        ; バッファ一覧
(global-set-key "\C-xm" 'man)                            ; man
(global-set-key "\C-xp" 'call-last-kbd-macro)            ; マクロ
(global-set-key "\M-?" 'help)                            ; ヘルプ
(global-set-key "\M-[" 'backward-paragraph)              ; 前のパラグラフへ移動
(global-set-key "\M-]" 'forward-paragraph)               ; 次のパラグラフへ移動
(global-set-key "\M-g" 'goto-line)                       ; 指定行へジャンプ
(global-set-key "\M-p" 'call-last-kbd-macro)             ; マクロ
(global-set-key "\M-y" 'browse-yank)                     ; 貼り付け拡張
(global-set-key [?\C-,] 'scroll-up-one-line)             ; 1行上へスクロール
(global-set-key [?\C-.] 'scroll-down-one-line)           ; 1行下へスクロール
(global-set-key "\C-x'" 'just-one-space)
(global-set-key "\M- " 'expand-abbrev)
(global-set-key "\C-ci" 'my-insert-filename)          ; ファイル名を挿入する

;; Emacs Lisp
(autoload 'auto-elc-mode "auto-elc-mode")
(add-hook 'emacs-lisp-mode-hook (lambda () (auto-elc-mode 1)))

;; Make
(setq auto-mode-alist
      (append
       '(("[Mm]akefile". makefile-gmake-mode)
         ("\.mk$". makefile-gmake-mode)
         ("\.d$". makefile-gmake-mode))
       auto-mode-alist))
(setq compilation-scroll-output 1)

;; lisp-interaction-mode
;; M-[space] でLisp補完
(define-key lisp-interaction-mode-map "\M- " 'lisp-complete-symbol)

;; shell-mode
(add-hook 'shell-mode-hook
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

;; shell-commandでコマンド入力に補完が効くようにする
(require 'shell-command)
(shell-command-completion-mode 1)

;; dired
(add-hook 'dired-load-hook
          (lambda ()
            ;; sorter - diredでのソート
            (load "sorter")
            ;; dired-x - diredの拡張機能
            (load "dired-x")
            ;; wdired - ファイル名の編集を可能にする
            (require 'wdired)
            ;;
            (put 'dired-find-alternate-file 'disabled nil)
            ;; ediff
            (defun dired-ediff-vc-latest-current ()
              "Run Ediff of file named on this line by comparing the latest version and current."
              (interactive)
              (let ((find-file-run-dired nil))
                (find-file (dired-get-file-for-visit))
                (ediff-vc-latest-current)))
            ;; image-dired
            (require 'image-dired)
            ;; ediff-revison
            (define-key dired-mode-map "\C-cw" 'wdired-change-to-wdired-mode)
            (define-key dired-mode-map "E" 'dired-ediff-vc-latest-current)
            (define-key dired-mode-map "\C-ce" 'ediff-revision)))

;;; CC-Mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-default-style "k&r")
             (setq c-basic-offset 4)
             (setq indent-tab-mode nil)
             (gtags-mode 1)
             (gtags-make-complete-list)))

;; find documentation on GNU MP functions in programing C
(eval-after-load "info-look"
  '(let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
     (setcar (nthcdr 3 mode-value)
             (cons '("(gmp)Function Index" nil "^ -.* " "\\>")
                   (nth 3 mode-value)))))

;; gtags-mode: GNU GLOBAL
(autoload 'gtags-mode "gtags" "" t)

;; bison-mode
(autoload 'bison-mode "bison-mode")

(add-hook 'bison-mode-hook
          '(lambda ()
             (setq bison-decl-token-column 0)
             (setq bison-rule-enumeration-column 8)))

(add-to-list 'auto-mode-alist '("\\.yy?$" . bison-mode))

;; flex-mode
(autoload 'flex-mode "flex-mode")
(add-to-list 'auto-mode-alist '("\\.ll?$" . flex-mode))

;; Mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
;(eval-after-load "mew" '(require 'mew-browse))

(declare-function mew-path-to-folder "mew-func" (PATH))
(declare-function mew-summary-visit-folder "mew-summary4" (FOLDER &optional GOEND NO-LS))
(declare-function mew-summary-move-and-display "mew-exec" (MSG &optional REDISPLAY))

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
(add-to-list 'auto-mode-alist '("\\.mew$" . mew-open-mesg))

(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))

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

;; w3m
(require 'w3m-load)
(setq w3m-default-display-inline-images t)
(setq mew-use-w3m-minor-mode 1)

;; nxml-mode
(require 'nxml-mode)
(setq magic-mode-alist '(("<\\?xml " . nxml-mode)))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg$" . nxml-mode))

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq nxml-child-indent 0)
            (setq indent-tabs-mode nil)))

;; image-mode
(setq image-file-name-extensions '("png" "jpeg" "jpg" "gif" "tiff" "tif"))

;; css-mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)

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

;; CASL II
(add-to-list 'auto-mode-alist '("\\.casl?$" . asm-mode))

;; graphviz mode
(load "graphviz-dot-mode")

;; ESS
(require 'ess-site)
(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
;; R起動時にワーキングディレクトリを訊ねない
(setq ess-ask-for-ess-directory nil)

;; CSV mode
(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

; Maxima
; http://emacswiki.org/emacs/MaximaMode
(add-to-list 'load-path "/usr/local/share/maxima/5.29.1/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

;; Mediawiki
(require 'mediawiki)
(add-to-list 'auto-mode-alist '("\\.wiki$" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("^ja.wikipedia.org/w/index.php" . mediawiki-mode))

;; exopen-mode 外部プログラムでファイルを開く
(require 'exopen-mode)
(add-hook 'exopen-mode-hook
          '(lambda ()
          (setq exopen-suffix-cmd '((".dvi" . "pxdvi")))))

;; tex-mode
(add-hook 'tex-mode-hook
          '(lambda ()
             (setq skeleton-pair 1)
             (define-key latex-mode-map "\C-cpp" 'exopen-buffer-pdffile)
             (define-key latex-mode-map "\C-cpd" 'exopen-buffer-dvifile)))

(add-hook 'latex-mode-hook 'turn-on-reftex)

;; color-selection
(autoload 'list-hexadecimal-colors-display "color-selection"
  "Display hexadecimal color codes, and show what they look like." t)
(defalias 'color-selection 'list-hexadecimal-colors-display)

;; igrep
(require 'igrep)

;; top-mode
(require 'top-mode)

;; svg-clock
(autoload 'svg-clock "svg-clock" "Start/stop svg-clock" t)
