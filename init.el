;;;-*-Emacs-Lisp-*-
;; for emacs 24.3

;; load-pathを追加し、subdirs.elがある場合は読み込む
(dolist
    (path
     '(
       "~/.emacs.d"
       "~/.emacs.d/init.d"
       "~/.emacs.d/init.sys.d"
       "~/.emacs.d/site-lisp"
       ))
  (let ((default-directory (expand-file-name path)))
    (add-to-list 'load-path default-directory)
    (when (file-exists-p "subdirs.el")
      (load-library "subdirs"))))

;; ライブラリを読み込む
(dolist
    (feat
     '(
       ;; /usr/local/share/emacs/24.3/lisp
       ediff
       uniquify
       vc
       whitespace
       ;; ~/.emacs.d/site-lisp
       auto-elc-mode
       count-japanese
       exopen-mode
       flex-autopair
       insert-file-name
       javadoc-style-comment-mode
       mediawiki
       other-windows-plus
       scroll-one-line
       temp-buffer
       ucs-normalize
       uniq
       window-control
       ;; package
       package
       ))
  (if (not (locate-library (symbol-name feat)))
      (message "%s: not found." feat)
    (require feat)))

(eval-after-load "package"
  '(progn
     ;; パッケージアーカイブ追加
    (dolist
        (pack
         '(
           ("marmalade" . "http://marmalade-repo.org/packages/")
           ("melpa" . "http://melpa.milkbox.net/packages/")
           ))
      (add-to-list 'package-archives pack))
    ;; パッケージ初期化
    (package-initialize)))

;; autoloadの設定
(dolist
    (list
     '(
       (R-mode "ess-site" "Emacs Speaks Statistics mode")
       (bison-mode "bison-mode" "Major mode for editing bison/yacc files")
       (css-mode "css-mode" "Cascading Style Sheets (CSS) editing mode")
       (csv-mode "csv-mode" "Major mode for editing comma-separated value files.")
       (eukleides-mode "eukleides" "Major mode for editing Eukleides files")
       (flex-mode "flex-mode" "Major mode for editing flex files")
       (graphviz-dot-mode "graphviz-dot-mode" "Major mode for the dot language")
       (gtags-mode "gtags" "Toggle Gtags mode, a minor mode for browsing source code using GLOBAL.")
       (list-hexadecimal-colors-display "color-selection" "Display hexadecimal color codes, and show what they look like.")
       (magit-status "magit" "Interface to the version control system Git")
       (nxml-mode "nxml-mode" "Major mode for editing XML")
       (ruby-mode "ruby-mode" "Mode for editing ruby source files")
       (rubydb "rubydb3x" "ruby debug")
       (svg-clock "svg-clock" "Start/stop svg-clock")
       ))
  (let ((func (car list)) (file (nth 1 list)) (doc (nth 2 list)))
    (if (not (locate-library file))
        (message "%s: not found." file)
      (autoload func file doc t))))

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

;; バックアップファイルを作成する
(setq make-backup-files 1)

;; バックアップファイルは、~/backupに格納
(let ((dir
       "~/backup"
       ))
  (if (not (file-exists-p (expand-file-name dir)))
      (message "backup directory %s is not exist." dir)
    (setq backup-directory-alist `(("." . ,dir)))))

;; バックアップファイルにバージョン番号を付ける
(setq version-control 1)

;; 古いバックアップファイルを自動的に削除する
(setq delete-old-versions 1)

;; インデント
(setq-default indent-line-function 'tab-to-tab-stop)

;; 圧縮されたファイルを直接編集する
(auto-compression-mode)

;; kill-lineのとき、改行も含めて切り取り
(setq kill-whole-line t)

;; yank-popを有効にする
(setq yank-pop-change-selection t)

;; whitespace
(eval-after-load "whitespace"
  '(load-library "init-whitespace"))

;; バッファ全体の濁点分離を直す
(eval-after-load "ucs-normalize"
  '(load-library "init-nfc"))

;; Ediff
(eval-after-load "ediff"
  '(load-library "init-ediff"))

;; uniquify
(eval-after-load "uniquify"
  '(progn
     (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
     (setq uniquify-ignore-buffers-re "*[^*]+*")))

;; auto-elc
(eval-after-load "auto-elc"
  '(add-hook 'emacs-lisp-mode-hook '(turn-on-auto-elc)))

;; *compilation*バッファをスクロールして表示
(eval-after-load "compile"
  '(setq compilation-scroll-output 1))

;; M-<return> でLisp補完
(define-key lisp-mode-shared-map (kbd "<M-return>") 'lisp-complete-symbol)

;; lisp-modeでのタブの設定
(add-hook 'lisp-mode-hook
          '(lambda ()
             (setq indent-line-function 'lisp-indent-line)))

;; shell-mode
(eval-after-load "shell"
  '(progn
     (setq shell-prompt-pattern
           "[~/][~/A-Za-z0-9_^$!#%&{}`'.,:()-]* \\[[0-9:]+\\] *$ ")
     (load-library "init-shell")))

;; dired
(add-hook 'dired-load-hook '(load-library "init-dired"))

(dolist
    (ext
     '(".bak" ".d" ".fls" ".log" ".dvi" ".xbb" ".out" ".prev" ".aux_prev"
       ".toc_prev" ".lot_prev" ".lof_prev" ".bbl_prev" ".out_prev"
       ".idx" ".ind" ".idx_prev" ".ind_prev" ".ilg"))
  (add-to-list 'completion-ignored-extensions ext))

;;; CC-Mode
(eval-after-load "cc-mode"
  '(progn
     (setq c-default-style "k&r")
     (setq c-basic-offset 4)
     (gtags-mode 1)
     (load-library "gnu-mp")))

;; bison-mode
(eval-after-load "bison"
  '(progn
    (setq bison-decl-token-column 0)
    (setq bison-rule-enumeration-column 8)))

;; web-mode
(eval-after-load "web-mode"
  '(load-library "init-web-mode"))

;; mmm-mode
(eval-after-load "mmm-auto"
  '(load-library "init-mmm"))

;; image-mode
(setq image-file-name-extensions
      '("svg" "png" "jpeg" "jpg" "gif" "tiff" "tif"))

;; css-mode
(eval-after-load "css-mode"
  '(setq cssm-indent-function #'cssm-c-style-indenter))

;; ChangeLog
(setq user-full-name "Kazuhito Takagi")

(setq user-mail-address "j8takagi@nifty.com")

(eval-after-load "add-log"
  '(setq change-log-default-name "~/ChangeLog"))

;; R起動時にワーキングディレクトリを訊ねない
(eval-after-load "ess-site"
  '(setq ess-ask-for-ess-directory nil))

;; mediawiki
(eval-after-load "mediawiki"
  '(define-key mediawiki-mode-map (kbd "C-x C-s") 'save-buffer))

;; tex-mode
(eval-after-load "tex-mode"
  '(progn
    (define-key latex-mode-map (kbd "C-c p p") 'exopen-buffer-pdffile)
    (define-key latex-mode-map (kbd "C-c p d") 'exopen-buffer-dvifile)
    (add-hook 'latex-mode-hook 'turn-on-reftex)))

;; color-selection
(eval-after-load "color-selection"
  '(defalias 'color-selection 'list-hexadecimal-colors-display))

;; Ruby
(eval-after-load "ruby-mode"
  '(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)))

;; text-modeで<M-tab>でのispell起動を無効に
(eval-after-load "text-mode"
  '(define-key text-mode-map (kbd "C-M-i") nil))

;; Info
(eval-after-load "info"
  '(progn
     (dolist
         (path
          '(
            "/usr/local/share/info/ja"
            "~/share/info"
            ))
       (add-to-list 'Info-default-directory-list (expand-file-name path)))
     (setq Info-additional-directory-list Info-default-directory-list)))

;; magic-mode-alist
(dolist
    (alist
     '(
       ("<![Dd][Oo][Cc][Tt][Yy][Pp][Ee] [Hh][Tt][Mm][Ll]" . web-mode)
       ("<\\?xml " . nxml-mode)
       ))
  (let ((mode (cdr alist)))
    (if (not (functionp mode))
        (message "magic-mode-alist: %s is not defined." mode)
      (add-to-list 'magic-mode-alist mode))))

;; auto-mode-alist
(dolist
    (alist
     '(
       ("^\(GNU\)?[Mm]akefile". makefile-gmake-mode)
       ("\.d$". makefile-gmake-mode)
       ("\.mk$". makefile-gmake-mode)
       ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
       ("\\.[rR]$" . R-mode)
       ("\\.casl?$" . asm-mode)
       ("\\.css$" . css-mode)
       ("\\.euk$" . eukleides-mode)
       ("\\.gv$" . graphviz-dot-mode)
       ("\\.html?$" . web-mode)
       ("\\.js$" . js-mode)
       ("\\.ll?$" . flex-mode)
       ("\\.svg$" . nxml-mode)
       ("\\.wiki$" . mediawiki-mode)
       ("\\.xml$" . nxml-mode)
       ("\\.y?rb$" . ruby-mode)
       ("\\.yy?$" . bison-mode)
       ("^ja.wikipedia.org/w/index.php" . mediawiki-mode)
       ))
  (let ((mode (cdr alist)))
    (if (not (functionp mode))
        (message "auto-mode-alist: %s is not defined." mode)
      (add-to-list 'auto-mode-alist alist))))

;; global-key
(dolist
    (map
     '(
       ("<M-down>" windmove-down)
       ("<M-left>" windmove-left)
       ("<M-return>" expand-abbrev)
       ("<M-right>" windmove-right)
       ("<M-up>" windmove-up)
       ("C-," scroll-up-one-line)
       ("C-." scroll-down-one-line)
       ("C-c C-c" comment-region)
       ("C-c C-u" uncomment-region)
       ("C-c C-v" view-mode)
       ("C-c I" insert-file-name-abs)
       ("C-c c" compile)
       ("C-c g" magit-status)
       ("C-c i" insert-file-name)
       ("C-c t" switch-to-temp-buffer)
       ("C-c w t" whitespace-toggle-options)
       ("C-c w w" whitespace-mode)
       ("C-h TAB" info-lookup-symbol)
       ("C-j" newline)
       ("C-x '" just-one-space)
       ("C-x C-e" electric-buffer-list)
       ("C-x K" kill-buffer-and-window)
       ("C-x RET u" ucs-normalize-NFC-buffer)
       ("C-x m" man)
       ("C-x p" call-last-kbd-macro)
       ("C-x v e" ediff-vc-latest-current)
       ("C-x v f" find-file-revision)
       ("M-?" help)
       ("M-[" backward-paragraph)
       ("M-]" forward-paragraph)
       ("M-g" goto-line)
       ("M-p" call-last-kbd-macro)
       ("RET" newline-and-indent)
       ))
  (let ((key (car map)) (func (nth 1 map)))
    (if (not (functionp func))
        (message "%s is not defined." func)
      (global-set-key (kbd key) func))))

(dolist
    (key '("C-x C-d" "C-x 4 0"))
  (global-unset-key (kbd key)))

;; システムごとの設定
(dolist
     (list
     '(
       (system-type gnu/linux init-linux)
       (window-system ns init-mac)
       (window-system x init-x)
       (window-system w32 init-w32)
       ))
   (let ((target (car list)) (sys (nth 1 list)) (feat (nth 2 list)))
     (when (equal (eval target) sys)
       (if (not (locate-library (symbol-name feat)))
           (message "%s: not found." feat)
         (require feat)))))
