;;;-*-Emacs-Lisp-*-
;; for emacs 24.3
;;
;; パッケージは、~/.emacs.dディレクトリーのelpaとsite-lispで管理

;; load-pathを追加し、subdirs.elがある場合は読み込む
(dolist
    (path
     '(
       "~/.emacs.d"
       "~/.emacs.d/site-lisp"
       ))
  (let ((default-directory (expand-file-name path)))
    (add-to-list 'load-path default-directory)
    (when (file-exists-p "subdirs.el")
        (load-library "subdirs"))))

;; ライブラリ読み込み
(dolist
    (feat
     '(
       ;; /usr/local/share/emacs/24.3/lisp
       ediff nxml-mode uniquify vc whitespace
       ;; ~/.emacs.d/site-lisp
       eukleides exopen-mode flex-autopair graphviz-dot-mode
       javadoc-style-comment-mode other-windows-plus
       ucs-normalize uniq window-control
       ))
  (if (not (locate-library (symbol-name feat)))
      (message "%s: not found." feat)
    (require feat)))

;; パッケージを使う
(require 'package)

;; パッケージアーカイブ追加
(dolist
    (pack
     '(
       ("marmalade" . "http://marmalade-repo.org/packages/")
       ("melpa" . "http://melpa.milkbox.net/packages/")
       ))
    (add-to-list 'package-archives pack))

; パッケージ初期化
(package-initialize)

;; session
(add-hook 'after-init-hook 'session-initialize)

;; ffap-bindings
(ffap-bindings)

;; 日本語環境
(set-language-environment 'Japanese)

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

;; エラー時、音が鳴るのではなく、画面が点滅するように
(setq visible-bell 1)

;; メジャーモードに合わせた色を付ける font-lock-mode
(global-font-lock-mode 1)

;; ダイアログボックスは使わない
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;; タブの幅は、4
(setq-default tab-width 4)

;; タブをスペースに展開
(setq-default indent-tabs-mode nil)

;; 再帰的なミニバッファ
(setq enable-recursive-minibuffers 1)

;; ファイル末尾で、end of bufferエラーなしで改行
(setq next-line-add-newlines nil)

;; 行番号を表示
(line-number-mode 1)

;; 列番号を表示
(column-number-mode 1)

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

;; バックアップファイルは、~/backupに格納
(setq backup-dir (expand-file-name "~/backup"))

(if (not (file-exists-p backup-dir))
    (message "backup-dir %s is not exist." backup-dir)
  (setq backup-directory-alist `(("." . ,backup-dir)))
  (setq make-backup-files t)
  (setq version-control t)
  (setq delete-old-versions t))

;; インデント
(setq indent-line-function 'tab-to-tab-stop)

;; 圧縮されたファイルを直接編集する
(auto-compression-mode)

;; kill-lineのとき、改行も含めて切り取り
(setq kill-whole-line t)

;; kill-ring
(setq yank-pop-change-selection t)

(defun insert-file-name (filename)
  (interactive "*fInsert file name: ")
  (insert filename))

(defun insert-file-name-abs (filename)
  (interactive "*fInsert file name: ")
  (insert (expand-file-name filename)))

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

;; テンポラリバッファを開く
(defun switch-to-temp-buffer ()
  "Create temporary buffer."
  (interactive)
  ;; バッファ名は現在の日時
  (switch-to-buffer
   (generate-new-buffer
    (concat "*" (replace-regexp-in-string " +" "_" (current-time-string)) "*"))
  (setq buffer-offer-save nil)))

;; whitespace
(when (featurep 'whitespace)
  (setq whitespace-style '(face tabs spaces trailing)) ;  タブ
  (setq whitespace-space-regexp "\\(　\\)")     ;　全角スペース
  (setq whitespace-trailing-regexp "\\( +$\\)") ;行末の空白    

  (set-face-attribute whitespace-tab nil :box "navy" :background (background-color-at-point))
  (set-face-attribute whitespace-space nil :box "orange" :background (background-color-at-point))
  (set-face-attribute whitespace-trailing nil
    :foreground "navy" :background (background-color-at-point) :underline "navy")

  ;; whitespaceを無効にするメジャーモード
  (defvar whitespace-disabled-major-mode-list)
  (setq whitespace-disabled-major-mode-list
    '(mew-summary-mode completion-list-mode help-mode
    magit-mode tetris-mode w3m-mode mew-message-mode))

  ;; メジャーモード設定後、バッファーが読み取り専用でない場合はwhitespaceを有効にする
  (add-hook 'after-change-major-mode-hook
    '(lambda ()
      (unless (or buffer-read-only (member major-mode whitespace-disabled-major-mode-list))
        (whitespace-mode 1)))))

;; Ediff
(if (not (featurep 'ediff))
    (message "ediff is not loaded.")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

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
        (ediff-vc-internal "" "")))))

  (defun find-file-revision (&optional file revision)
    "find-file FILE REVISION.
Input FILE first, REVISION then.
Or, input FILE as 'FILE.~REVISON~' and FILE and REVISION is specified."
    (interactive "P")
    (unless (stringp file)
      (setq file (expand-file-name (read-file-name "Find version controled file: "))))
    ;; find-file FILE REVISION by 'FILE.~REVISION~'."
    (when (string-match "\\(.+\\)\\.~\\(.+\\)~$" file)
      (setq revision (substring file (match-beginning 2) (match-end 2)))
      (setq file (substring file (match-beginning 1) (match-end 1))))
    (unless (vc-backend file)
      (error (format "%s is not under version control." file)))
    (unless (stringp revision)
      (setq revision
            (vc-read-revision
             (format
              "Revision (default %s's working revision): " (file-name-nondirectory file))
             (list file))))
    (when (string= revision "")
      (setq revision nil))
    (set-buffer (find-file-noselect file))
    (switch-to-buffer (vc-find-revision file revision)))

;; magit-mode
(autoload 'magit-status "magit" nil t)

;; uniquify
(when (featurep 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; auto-elc
(autoload 'auto-elc-mode "auto-elc-mode")
(add-hook 'emacs-lisp-mode-hook
  '(lambda () (auto-elc-mode 1)))

;; *compilation*バッファをスクロールして表示
(setq compilation-scroll-output 1)

;; lisp-interaction-mode, emacs-lisp-mode
;; M-<return> でLisp補完
(define-key lisp-interaction-mode-map (kbd "<M-return>") 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map (kbd "<M-return>") 'lisp-complete-symbol)

;; shell-mode
(add-hook 'shell-mode-hook
 '(lambda ()
    (setq shell-prompt-pattern "[~/][~/A-Za-z0-9_^$!#%&{}`'.,:()-]* \\[[0-9:]+\\] *$ ")
    (setq tab-width 4)))

;; 引数で指定されたプロセスの名前が shell で子プロセスがない場合は、
;; process-query-on-exit-flag を nil に設定し、
;; "Buffer has a runnig process.; kill it?"
;; のプロンプト表示を抑制する。
(defun set-process-not-running-child-noquery-on-exit (proc)
  (when (and proc (string= (process-name proc) "shell"))
      (set-process-query-on-exit-flag proc (process-running-child-p proc))))

(defadvice kill-buffer (before my-set-process-query activate)
  (set-process-not-running-child-noquery-on-exit (get-buffer-process (current-buffer))))

(defadvice save-buffers-kill-terminal (before my-set-process-query activate)
  (dolist (proc (process-list))
    (set-process-not-running-child-noquery-on-exit proc)))

;; dired
(add-hook
 'dired-load-hook
 (lambda ()
   ;; 確認なしにディレクトリーを再帰的にコピーする
   (setq dired-recursive-copies 'always)
   ;; sorter - diredでのソートo
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

(dolist
    (ext
     '(".bak" ".d" ".fls" ".log" ".dvi" ".xbb" ".out" ".prev" ".aux_prev"
       ".toc_prev" ".lot_prev" ".lof_prev" ".bbl_prev" ".out_prev"
       ".idx" ".ind" ".idx_prev" ".ind_prev" ".ilg"))
     (add-to-list 'completion-ignored-extensions ext))

;;; CC-Mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-default-style "k&r")
             (setq c-basic-offset 4)
             (setq indent-tab-mode nil)
             (gtags-mode 1)))

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

;; flex-mode
(autoload 'flex-mode "flex-mode")

;; 色の設定
(custom-set-faces
 '(web-mode-indent-style 1)
 '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))

;; mmm-mode
(require 'mmm-auto)

(setq mmm-global-mode 'maybe)

(setq mmm-submode-decoration-level 3)

(set-face-background 'mmm-default-submode-face "#f0f0ff")

;;
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>\n"
    :back "\n?[ \t]+</style>")))

;;
(mmm-add-mode-ext-class nil "\\.html?\\'" 'embedded-css)

;;
(mmm-add-classes
 '((html-javascript
    :submode javascript-mode
    :front "<script[^>]*>\n"
    :back "[ \t]+</script>")))

(mmm-add-mode-ext-class nil "\\.html?\\'" 'html-javascript)

;; image-mode
(setq image-file-name-extensions '("svg" "png" "jpeg" "jpg" "gif" "tiff" "tif"))

;; css-mode
(autoload 'css-mode "css-mode")
(setq cssm-indent-function #'cssm-c-style-indenter)

;; ChangeLog
(setq user-full-name "高木和人")
(setq user-mail-address "j8takagi@nifty.com")
(setq change-log-default-name "~/ChangeLog")

(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)

;; R起動時にワーキングディレクトリを訊ねない
(setq ess-ask-for-ess-directory nil)

;; CSV mode
(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)

(add-hook 'mediawiki-mode-hook
          '(lambda ()
             (define-key mediawiki-mode-map "\C-x\C-s" 'save-buffer)))


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

;; svg-clock
(autoload 'svg-clock "svg-clock" "Start/stop svg-clock" t)

;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; rubydb - ruby debugger
(autoload 'rubydb "rubydb3x" "ruby debug" t)

;; Riece IRC client
(autoload 'riece "riece" "Start Riece" t)

(prefer-coding-system 'utf-8-hfs)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

(defun ucs-normalize-NFC-buffer ()
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max)))

;; text-modeで<M-tab>でのispell起動を無効に
(add-hook 'text-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-M-i"))))

(add-hook 'Info-mode-hook       ; After Info-mode has started
          (lambda ()
            (setq Info-additional-directory-list Info-default-directory-list)
            ))

;; magic-mode-alist
(add-to-list 'magic-mode-alist '("<![Dd][Oo][Cc][Tt][Yy][Pp][Ee] [Hh][Tt][Mm][Ll]" . web-mode))
(add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode))

;; auto-mode-alist
(add-to-list 'auto-mode-alist '("[Mm]akefile$". makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\.d$". makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\.mk$". makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.casl?$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.euk$" . eukleides-mode))
(add-to-list 'auto-mode-alist '("\\.gv$" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ll?$" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.mew$" . mew-open-mesg))
(add-to-list 'auto-mode-alist '("\\.svg$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.wiki$" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.y?rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.yy?$" . bison-mode))
(add-to-list 'auto-mode-alist '("^ja.wikipedia.org/w/index.php" . mediawiki-mode))

;; global-key
(global-set-key (kbd "<M-down>")  'windmove-down)             ; ウィンドウ移動
(global-set-key (kbd "<M-left>")  'windmove-left)             ; ウィンドウ移動
(global-set-key (kbd "<M-return>") 'expand-abbrev)
(global-set-key (kbd "<M-right>") 'windmove-right)            ; ウィンドウ移動
(global-set-key (kbd "<M-up>")    'windmove-up)               ; ウィンドウ移動
(global-set-key (kbd "C-,") 'scroll-up-one-line)              ; 1行上へスクロール
(global-set-key (kbd "C-.") 'scroll-down-one-line)            ; 1行下へスクロール
(global-set-key (kbd "C-c C-c") 'comment-region)              ; コメントを付ける
(global-set-key (kbd "C-c C-u") 'uncomment-region)            ; コメントを外す
(global-set-key (kbd "C-c C-v") 'view-mode)                   ; View mode
(global-set-key (kbd "C-c c") 'compile)                       ; make
(global-set-key (kbd "C-c g") 'magit-status)                  ; magit
(global-set-key (kbd "C-c i") 'insert-file-name)              ; ファイル名を挿入する
(global-set-key (kbd "C-c I") 'insert-file-name-abs)          ; ファイルのフルパスを挿入する
(global-set-key (kbd "C-c t") 'switch-to-temp-buffer)         ; テンポラリバッファを開く
(global-set-key (kbd "C-c w t") 'whitespace-toggle-options)   ; whitespace-toggle-options
(global-set-key (kbd "C-c w w") 'whitespace-mode)             ; whitespace-mode
(global-set-key (kbd "C-h TAB") 'info-lookup-symbol)          ; SYMBOLのInfoを表示
(global-set-key (kbd "C-j") 'newline)                         ; C-jで、インデントなし改行
(global-set-key (kbd "C-x '") 'just-one-space)                ; 複数のスペースを1つに
(global-set-key (kbd "C-x C-e") 'electric-buffer-list)        ; バッファ一覧
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)        ; 現在のバッファとウィンドウを削除
(global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)  ; バッファ全体の濁点分離を直す
(global-set-key (kbd "C-x m") 'man)                           ; man
(global-set-key (kbd "C-x p") 'call-last-kbd-macro)           ; マクロ
(global-set-key (kbd "C-x v e") 'ediff-vc-latest-current)     ; 最新版と現在のファイルでEdiff
(global-set-key (kbd "C-x v f") 'find-file-revision)          ; ファイル旧版を開く
(global-set-key (kbd "M-?") 'help)                            ; ヘルプ
(global-set-key (kbd "M-[") 'backward-paragraph)              ; 前のパラグラフへ移動
(global-set-key (kbd "M-]") 'forward-paragraph)               ; 次のパラグラフへ移動
(global-set-key (kbd "M-g") 'goto-line)                       ; 指定行へジャンプ
(global-set-key (kbd "M-p") 'call-last-kbd-macro)             ; マクロ
(global-set-key (kbd "RET") 'newline-and-indent)              ; RETで、インデント付き改行

(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-x 4 0"))

;; OSごとの設定
(case system-type
  ('gnu/linux (require 'init-linux)))

; Windowシステムごとの設定
(case window-system
  ('ns (require 'init-mac))
  ('x (require 'init-x))
  ('w32 (require 'init-w32)))
