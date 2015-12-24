;;;-*-Emacs-Lisp-*-

(message
 "Loading init.el: Starting initialization %s at %s"
 (emacs-version nil)
 (format-time-string "%Y/%m/%d %T"))

(defun my-init-require (feature)
  "Require FEATURE, and the result is written into the `*Messages*' buffer."
  (if (require feature nil 1)
      (message "Feature `%s' is required." feature)
    (if (not (locate-library (symbol-name feature)))
        (message "Warning: required feature `%s' is NOT found." feature)
      (message "Warning: it fails to require feature `%s'" feature))))

;; load-pathの設定
(let ((default-directory (expand-file-name user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;;;
;;; パッケージ
;;;

(my-init-require 'package)

;; パッケージアーカイブ追加
(dolist
    (arch
     '(
       ("melpa-stable" . "http://stable.melpa.org/packages/")
       ))
  (add-to-list 'package-archives arch))

;; パッケージ初期化
(package-initialize)

;; インストールするパッケージ
(let (pkgs)
  (dolist
      (pkg
       '(
         csv-mode
         dash
         ess
         ggtags
         git-commit
         gitignore-mode
         gnuplot
         inf-ruby
         magit
         magit-popup
         markdown-mode
         mew
         mmm-mode
         session
         sokoban
         undo-tree
         web-mode
         with-editor
         xpm
         ))
    (when (not (package-installed-p pkg))
      (when (not package-archive-contents)
        (package-refresh-contents))
      (if (not (assq pkg package-archive-contents))
          (message "Warning: package `%s' is NOT installed and NOT found on archives." pkg)
        (message "Package `%s' is NOT installed. Installation begins." pkg)
        (condition-case err
            (package-install pkg)
          (message "Error: %s" err))))
    (add-to-list 'pkgs pkg))
  (let ((pkglist (mapcar 'car package-alist)))
    (message "Installed packages: %s" (reverse pkglist))
    (dolist (pkg pkgs)
      (setq pkglist (delete pkg pkglist)))
    (when pkglist
      (message "Unexpected installed packages: %s"  (reverse pkglist)))))

;;
;; ライブラリの読み込み
;;

(dolist
    (feat
     '(
       ;; /usr/local/share/emacs/${VERSION}/lisp
       autoinsert
       server
       skeleton
       uniquify
       ;; ~/.emacs.d/site-lisp
       auto-elc-mode
       byte-compile-buffer-file
       count-japanese
       exopen-mode
       mediawiki
       other-window-bindings
       scroll-one-line
       temp-buffer
       ucs-normalize
       window-control
       ))
  (my-init-require feat))

;; autoloadの設定
(let (funcs)
  (dolist
      (list
       '(
         (R-mode "ess-site" "Emacs Speaks Statistics mode")
         (bison-mode "bison-mode" "Major mode for editing bison/yacc files")
         ;; (css-mode "css-mode" "Cascading Style Sheets (CSS) editing mode")
         ;; (csv-mode "csv-mode" "Major mode for editing comma-separated value files.")
         (eukleides-mode "eukleides" "Major mode for editing Eukleides files")
         (flex-mode "flex-mode" "Major mode for editing flex files")
         (graphviz-dot-mode "graphviz-dot-mode" "Major mode for the dot language")
         ;; (js-mode "js" "Major mode for editing JavaScript.")
         (list-hexadecimal-colors-display "color-selection" "Display hexadecimal color codes, and show what they look like.")
         (mpv-ts-mode "mpv-ts" "transcription using mpv")
         (nxml-mode "nxml-mode" "Major mode for editing XML")
         (review-mode "review-mode" "Re:VIEW text editing mode")
         (ruby-mode "ruby-mode" "Mode for editing ruby source files")
         (rubydb "rubydb3x" "ruby debug")
         (svg-clock "svg-clock" "Start/stop svg-clock")
         ))
    (let ((func (car list)) (file (nth 1 list)) (doc (nth 2 list)))
      (if (not (locate-library file))
          (message "Warning: library file `%s' autoloaded from `%s' is not found." file func))
      (when (autoload func file doc 1)
        (add-to-list 'funcs func))))
  (message "Autoload functions: %s" (reverse funcs)))

;;
;; 文字コードの設定
;;

;; 日本語環境
(set-language-environment 'Japanese)
;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8)

;; マイナーモードの設定
(dolist
    (mode
     '(
       ;; 有効にするマイナーモード
       (abbrev-mode 1)              ; Abbrevsを使う
       (auto-compression-mode 1)    ; 圧縮されたファイルを直接編集する
       (column-number-mode 1)       ; 列番号を表示
       (global-auto-revert-mode 1) ; ファイルが外部で更新されたとき、バッファ自動再読み込み
       (global-font-lock-mode 1)  ; メジャーモードに合わせた色を付ける
       (line-number-mode 1)       ; 行番号を表示
       (show-paren-mode 1)        ; 括弧の対応を表示
       (transient-mark-mode 1)    ;リージョンをハイライト
       ;; 無効にするマイナーモード
       (blink-cursor-mode 0)        ; カーソルは点滅しない
       (electric-indent-mode 0) ; 改行時の自動インデントを無効に（Emacs24から、初期値が有効）
       (menu-bar-mode 0)          ; メニューバーを表示しない
       (tool-bar-mode 0)          ; ツールバーを表示しない
       ))
  (if (not (fboundp (car mode)))
      (message "%s is not defined." (car mode))
    (eval mode)))

;; メッセージダイアログボックスは使わない
(defalias 'message-box 'message)

;; カスタムデフォルト値の設定
(dolist
    (varval
     '(
       (indent-tabs-mode nil)           ;; タブをスペースに展開
       (tab-width 4))                   ;; タブ幅は4
       )
  (let ((var (car varval)) (val (nth 1 varval)))
    (custom-set-default var val)))

;; インデント
(setq-default indent-line-function 'indent-to-left-margin)

;; カスタム変数の設定
(custom-set-variables
 ;; 起動時の画面を表示しない
 '(inhibit-startup-screen 1)
 ;; すべてのコマンド（narrow-to-region、erase-bufferなど）の使用制限を解除する
 '(disabled-command-function nil)
 ;; 履歴の数を増やす
 '(history-length t)
 ;; *scratch* のメッセージを表示しない
 '(initial-scratch-message nil)
 ;; 重複する履歴は削除
 '(history-delete-duplicates 1)
 ;; エラー時、音が鳴るのではなく、画面が点滅するように
 '(visible-bell 1)
 ;; ダイアログボックスは使わない
 '(use-dialog-box nil)
 ;; 再帰的なミニバッファ
 '(enable-recursive-minibuffers 1)
 ;; ファイル末尾での改行で、end of bufferエラーが発生しないように
 '(next-line-add-newlines nil)
 ;; 行の折り返しをしない
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 ;; kill-lineのとき、改行も含めて切り取り
 '(kill-whole-line 1)
 ;; 置換時に大文字小文字を区別しない
 '(case-replace nil)
 ;; 画面最下部で下向き、画面最上部で上向きにスクロールするとき、1行ずつスクロール
 '(scroll-conservatively 1)
 ;; バックアップファイルを作成する
 '(make-backup-files 1)
 ;; バックアップファイルにバージョン番号を付ける
 '(version-control 1)
 ;; 古いバックアップファイルを自動的に削除する
 '(delete-old-versions 1)
 ;; kill-lineのとき、改行も含めて切り取り
 '(kill-whole-line 1)
 ;; yank-popを有効にする
 '(yank-pop-change-selection 1)
 ;; evalした結果を全部表示する
 '(eval-expression-print-length nil)
 ;; ChangeLogなどの設定
 '(user-mail-address "j8takagi@nifty.com")
)

;; *scratch* と *Messages* のバッファを削除しない
(my-init-require 'init-scratch-messages)

;; フレームの設定
(unless (equal window-system nil)
  (dolist
      (val
       '(
         (foreground-color . "black")
         (background-color . "gray99")
         (cursor-color . "DarkOliveGreen")
         (cursor-type . box)
         ))
    (add-to-list 'default-frame-alist val)))

;; ファイル名の補完入力の対象から外す拡張子。diredで淡色表示される
(dolist
    (ext
     '(
       ".bak" ".d" ".fls" ".log" ".dvi" ".xbb" ".out" ".prev" ".aux_prev"
       ".toc_prev" ".lot_prev" ".lof_prev" ".bbl_prev" ".out_prev"
       ".idx" ".ind" ".idx_prev" ".ind_prev" ".ilg" "tmp" ".synctex.gz" ".DS_Store"
       "dplg" "dslg"
       ))
  (add-to-list 'completion-ignored-extensions ext))

;; バックアップファイルの保存先
(dolist
     (ptndir
      '(
        ("." . "~/backup")
        ))
   (let ((dir (expand-file-name (cdr ptndir))))
     (if (not (file-directory-p dir))
         (message "Warning: backup directory `%s' is NOT exist." dir)
       (setcdr ptndir dir)
       (add-to-list 'backup-directory-alist ptndir))))

;; Info
(eval-after-load "info"
  '(progn
     (custom-set-variables '(Info-directory-list (reverse Info-directory-list)))
     (dolist
       (path
        '(
          "/usr/local/share/info/ja"
          "~/share/info/ja"
          "~/share/info"
          ))
     (let ((fullpath (expand-file-name path)))
       (if (not (car (file-attributes fullpath)))
           (message "Warning: path `%s' is not exist or not directory." path)
         (add-to-list 'Info-directory-list fullpath 1)
         )))))

;; dired
(eval-when-compile (load "dired"))
(eval-after-load "dired"
  '(my-init-require 'init-dired)
  )

;; view-modeの設定
(eval-after-load "view"
  '(progn
     ;; read-onlyファイルをview-modeで開く
     (my-init-require 'init-view-mode)
     (custom-set-variables '(view-read-only 1))
     ;; view-modeでviのキーバインド
     (my-init-require 'view-mode-vi-bindings)))

;; バッファ全体の濁点分離を直す
(eval-after-load "ucs-normalize" '(my-init-require 'init-nfc))

;; lisp-mode
(eval-after-load "lisp-mode"
  '(progn                               ; タブの設定
     (defun my-init-indent-lisp-indent-line ()
       (setq indent-line-function 'lisp-indent-line))
     (add-hook 'emacs-lisp-mode-hook 'my-init-indent-lisp-indent-line)))

;; auto-elc-mode
(eval-after-load "auto-elc-mode"
  '(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-elc))

;; Ediff
(eval-after-load "ediff"
  '(my-init-require 'init-ediff))

;; uniquify
(eval-after-load "uniquify"
  (custom-set-variables
   '(uniquify-buffer-name-style 'post-forward-angle-brackets)
   '(uniquify-ignore-buffers-re "*[^*]+*")))

;; *compilation*バッファをスクロールして表示
(eval-when-compile (load "compile"))
(eval-after-load "compile"
  '(custom-set-variables '(compilation-scroll-output 'first-error)))

;; autoinsert
;; 参考: http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/autoinsert.html
(eval-after-load "autoinsert"
  '(progn
     (add-hook 'find-file-hook 'auto-insert)
     (custom-set-variables
      '(auto-insert-directory "~/.emacs.d/insert/")
      '(auto-insert-query nil)
      '(auto-insert-alist nil))
     (my-init-require 'skeleton-file-name)
     (my-init-require 'skeleton-pair-japanese)
     (dolist
         (libskel
          '(
            ("cc-mode" c-skeletons)
            ("cc-mode" h-skeletons)
            ("lisp-mode" emacs-lisp-skeletons)
            ("tex-mode" latex-skeletons)
            ("web-mode" web-skeletons)
            ("graphviz-dot-mode" graphviz-dot-skeletons)
            ))
       (let ((lib (car libskel)) (skel (nth 1 libskel)))
         (eval-after-load lib
           `(my-init-require ',skel))))))

;; session
(add-hook 'after-init-hook 'session-initialize)

;; emacsclient
(eval-after-load "server"
  '(unless (server-running-p) (server-start)))

;; ChangeLog
(eval-when-compile (load "add-log"))
(eval-after-load "add-log"
  '(custom-set-variables '(change-log-default-name "~/ChangeLog")))

;; vc-follow-linkを無効にする
;; 参考: http://d.hatena.ne.jp/a_bicky/20140607/1402131090
(eval-when-compile (load "vc-hooks"))
(eval-after-load "vc-hooks"
  '(custom-set-variables '(vc-follow-symlinks nil)))

;; whitespace
(eval-after-load "whitespace"
  '(my-init-require 'init-whitespace))

;; shell-mode
(eval-when-compile (load "shell"))
(eval-after-load "shell"
  '(progn
     (custom-set-variables              ; プロンプトの表示設定
      '(shell-prompt-pattern
        "[~/][~/A-Za-z0-9_^$!#%&{}`'.,:()-]* \\[[0-9:]+\\] *$ "))
     (my-init-require 'init-shell)))

;;; CC-Mode
(eval-when-compile (load "cc-mode"))
(eval-after-load "cc-mode"
  '(progn
     (custom-set-variables '(c-default-style "k&r"))
     (custom-set-variables '(c-basic-offset 4))
     (defun my-init-cc-ggtags-mode-turnon ()
       (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
         (ggtags-mode 1)))
     (defun my-init-require-gnu-mp ()
       (when (derived-mode-p 'c-mode 'c++-mode)
         (my-init-require 'gnu-mp)))
     (dolist
         (func
          '(
            my-init-cc-ggtags-mode-turnon
            my-init-require-gnu-mp
            ))
       (add-hook 'c-mode-common-hook func))))

;; tex-mode
(eval-when-compile (load "tex-mode"))
(eval-after-load "tex-mode"
  '(add-hook 'latex-mode-hook 'turn-on-reftex))

;; web-mode
(eval-when-compile (load "web-mode"))
(eval-after-load "web-mode"
  '(my-init-require 'init-web-mode))

;; mmm-mode
(eval-when-compile (load "mmm-auto"))
(eval-after-load "mmm-auto"
  '(my-init-require 'init-mmm))

;; image-mode
(eval-when-compile (load "image-file"))
(eval-after-load "image-file"
  (custom-set-variables
   '(image-file-name-extensions
     '(
       "svg" "png" "jpeg" "jpg" "gif" "tiff" "tif"
       ))))

;; ess-site > R
(eval-when-compile (load "ess-site"))
(eval-after-load "ess-site"
  '(custom-set-variables '(ess-ask-for-ess-directory nil)))

;; bison-mode
(eval-when-compile (load "bison-mode"))
(eval-after-load "bison-mode"
  '(custom-set-variables
      '(bison-decl-token-column 0)
      '(bison-rule-enumeration-column 8)))

;; graphviz-dot-mode
(eval-when-compile (load "graphviz-dot-mode"))
(eval-after-load "graphviz-dot-mode"
  '(progn
     (defun my-init-graphviz-dot-mode-set-make-compile-command ()
       (make-local-variable 'compile-command)
       (custom-set-variables '(compile-command "make -k")))
     (add-hook 'graphviz-dot-mode-hook
             'my-init-graphviz-dot-mode-set-make-compile-command)))

;; color-selection
(defalias 'color-selection 'list-hexadecimal-colors-display)

;; magit
(eval-when-compile (load "magit"))
(eval-after-load "magit"
  '(custom-set-variables '(magit-status-buffer-switch-function 'switch-to-buffer)))

;; mew
(eval-when-compile (load "mew"))
(custom-set-variables '(read-mail-command 'mew))
(define-mail-user-agent
  'mew-user-agent
  'mew-user-agent-compose
  'mew-draft-send-message
  'mew-draft-kill
  'mew-send-hook)

;; ruby-mode
(eval-when-compile (load "ruby-mode"))
(eval-after-load "ruby-mode"
  '(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)))

;;
;; ファイルの自動判定
;;

;; magic-mode-alist
(dolist
    (alist
     '(
       ("<![Dd][Oo][Cc][Tt][Yy][Pp][Ee] [Hh][Tt][Mm][Ll]" . web-mode)
       ("<\\?xml " . nxml-mode)
       ))
  (let ((mode (cdr alist)))
    (if (not (functionp mode))
        (message "Warning (magic-mode-alist): function `%s' is not defined." mode)
      (add-to-list 'magic-mode-alist alist 1))))

;; auto-mode-alist
;; 既存のモード設定を上書きする
(dolist
    (list
     '(
       (makefile-gmake-mode makefile-bsdmake-mode)
       (web-mode html-mode)
       ))
  (let ((newmode (car list)) (oldmode (nth 1 list)))
    (if (not (and (functionp oldmode) (functionp newmode)))
        (message "Warning (auto-mode-alist): function `%s' or `%s' is not defined." newmode oldmode)
      (while
          (let ((alist (rassoc oldmode auto-mode-alist)))
            (when alist
                (setcdr alist newmode)))))))

;; 新しいモード設定を追加する
(dolist
    (alist
     '(
       ("Makefile\\.?.*". makefile-gmake-mode)
       ("\\.[CcTt][Ss][Vv]\\'" . csv-mode)
       ("\\.[rR]\\'" . R-mode)
       ("\\.casl?\\'" . asm-mode)
       ("\\.d\\'". makefile-gmake-mode)
       ("\\.euk\\'" . eukleides-mode)
       ("\\.gp\\'" . gnuplot-mode)
       ("\\.gv\\'" . graphviz-dot-mode)
       ("\\.ll?\\'" . flex-mode)
       ("\\.md\\'" . markdown-mode)
       ("\\.mk\\'". makefile-gmake-mode)
       ("\\.re\\'" . review-mode)
       ("\\.svg\\'" . nxml-mode)
       ("\\.ts\\'" . mpv-ts-mode)
       ("\\.wiki\\'" . mediawiki-mode)
       ("\\.xml\\'" . nxml-mode)
       ("\\.y?rb\\'" . ruby-mode)
       ("\\.yy?\\'" . bison-mode)
       ("\\`ja.wikipedia.org/w/index.php" . mediawiki-mode)
       ("abbrev_defs" . emacs-lisp-mode)
       ("cmd" . shell-script-mode)
       ))
  (let ((mode (cdr alist)))
    (if (not (functionp mode))
        (message "Warning (auto-mode-alist): function `%s' is not defined." mode)
      (add-to-list 'auto-mode-alist alist))))

;;
;; キーバインド
;;

;; global-key
(dolist
    (mapkeys
     '(
       ("<M-down>" windmove-down)
       ("<M-left>" windmove-left)
       ("<M-return>" expand-abbrev)
       ("<M-right>" windmove-right)
       ("<M-up>" windmove-up)
       ("C-' h" windmove-left)
       ("C-' j" windmove-down)
       ("C-' k" windmove-up)
       ("C-' l" windmove-right)
       ("C-," scroll-up-one-line)
       ("C-." scroll-down-one-line)
       ("C-`" expand-abbrev)
       ("C-c C-c" comment-region)
       ("C-c C-u" uncomment-region)
       ("C-c C-v" view-mode)
       ("C-c c" compile)
       ("C-c g" magit-status)
       ("C-c t" switch-to-temp-buffer)
       ("C-c w t" whitespace-toggle-options)
       ("C-c w w" whitespace-mode)
       ("C-h TAB" info-lookup-symbol)
       ("C-j" newline)
       ("C-x '" just-one-space)
       ("C-x C-M-b" electric-buffer-list)
       ("C-x K" kill-buffer-and-window)
       ("C-x RET u" ucs-normalize-NFC-buffer)
       ("C-x m" man)
       ("C-x p" call-last-kbd-macro)
       ("C-x q" bury-buffer)
       ("C-x v e" ediff-vc-latest-current)
       ("C-x v f" find-file-revision)
       ("M-?" help)
       ("M-[" backward-paragraph)
       ("M-]" forward-paragraph)
       ("M-p" call-last-kbd-macro)
       ("RET" newline-and-indent)
       ))
  (let ((key (car mapkeys)) (func (nth 1 mapkeys)))
    (if (not (functionp func))
        (message "Warning: function `%s' is NOT defined." func)
      (global-set-key (kbd key) func))))

;; ffap（find file at point）のキーバインド
(ffap-bindings)

;; ウィンドウやバッファに関するキーバインド
(other-window-bindings)

;; 無効にするキーバインド
(dolist
    (key
     '(
       "C-x C-d"                         ; ffap-list-directory を無効に
       "C-x 4 0"                         ; kill-buffer-and-window を無効に
       "M-`"                             ; tmm-menubar を無効に
       ))
  (global-unset-key (kbd key)))

;; モードごとのキーバインドを設定
;; 設定時、関数 my-init-<mode-map-name>-keybind を定義し、フックに追加する
;; リストの形式: (mode-library mode-hook mode-map-name ((key1 function1) (key2 function2)))
(dolist
    (list
     '(
       ("text-mode" text-mode-hook text-mode-map
        (
         ("C-M-i" dabbrev-expand) ; ispell 起動を無効にし、dabbrev-expand を設定
         ))
       ("tex-mode" latex-mode-hook latex-mode-map
        (
         ("<M-return>" latex-insert-item) ; latex-insert-itemを再設定
         ("C-c p p" exopen-buffer-pdffile)
         ("C-c p d" exopen-buffer-dvifile)
         ("C-c C-c" comment-region)     ; tex-compileを無効にし、comment-region を設定
         ))
       ("lisp-mode" emacs-lisp-mode-hook lisp-mode-shared-map
        (
         ;; ("<M-return>" noexist)      ; デバッグ用
         ("<M-return>" completion-at-point)
         ))
       ("mediawiki" mediawiki-mode-hook mediawiki-mode-map
        (
         ("C-x C-s" save-buffer)
         ))
       ))
  (let* ((lib (car list)) (hook (nth 1 list))
         (modemap (nth 2 list)) (mapkeys (nth 3 list))
         (modemap-name (symbol-name modemap))
         (func-init-keybind (read (concat "my-init-" modemap-name "-keybind"))))
    (eval-after-load lib
      (progn
        (fset
         func-init-keybind
         `(lambda ()
            (dolist
                (map ',mapkeys)
              (let ((key (car map)) (func (nth 1 map)))
                (if (not (functionp func))
                    (message
                     "Warning: In setting %s, function `%s' is not defined."
                     ,modemap-name func)
                  (define-key ,modemap (kbd key) func))))))
        `(add-hook ',hook ',func-init-keybind)))))

;;
;; システムごとの設定
;;
(defvar system-name-simple
  (replace-regexp-in-string "\\..*\\'" "" (system-name))
  "The simple host name of the machine Emacs is running on, which is without domain information.")

(dolist
     (condi
     '(
       (system-type gnu/linux init-linux)
       (system-type darwin init-darwin)
       (window-system mac init-mac-gui)
       (window-system x init-x)
       (window-system w32 init-w32)
       (system-name-simple "tiger" init-tiger)
       ))
   (let ((func (car condi)) (sys (nth 1 condi)) (feat (nth 2 condi)))
     (when (equal (eval func) sys)
       (my-init-require feat))))

;;
;; Emacs開始にかかった時間をメッセージに表示
;;
(defun my-init-message-startup-time ()
  (message "Duration of the Emacs initialization: %s" (emacs-init-time)))

(add-hook 'after-init-hook 'my-init-message-startup-time)
