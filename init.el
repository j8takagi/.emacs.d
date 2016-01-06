;;;-*-Emacs-Lisp-*-

(message "Start of loading init.el at %s." (format-time-string "%Y/%m/%d %T"))

(message (emacs-version nil))

;; (message "Debug: features before loading init.el - %s" features)

;; load-pathの設定
(let ((default-directory (expand-file-name user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;;;
;;; パッケージ
;;;

(my-init-require 'package)

(dolist                                 ; パッケージアーカイブ
    (arch
     '(
       ("melpa-stable" . "http://stable.melpa.org/packages/")
       ))
  (add-to-list 'package-archives arch))

;; パッケージ初期化
(package-initialize)

;; (message "Debug: auto-mode-alist just after package initialize - %s" auto-mode-alist)

;; パッケージの設定
(let (pkgs req-pkgs real-pkgs update-pkgs)
  ;; 指定したパッケージがインストールされていない場合は、インストール実行
  (dolist                               ; インストールするパッケージ
      (req-pkg
       '(
         csv-mode
         ess
         ggtags
         gitignore-mode
         gnuplot
         graphviz-dot-mode
         inf-ruby
         magit
         markdown-mode
         mediawiki
         mew
         session
         sokoban
         undo-tree
         web-mode
         xbm-life
         xpm
         ))
    (add-to-list 'req-pkgs req-pkg)
    (dolist (pkg (my-init-install-package req-pkg))
      (when (and (cadr pkg) (not (member (car pkg) pkgs)))
        (message "Package `%s' is required from `%s'." (car pkg) (cadr pkg)))
      (add-to-list 'pkgs (car pkg))))
  (message "Required packages - %s" (reverse req-pkgs)) ; init.elで指定したパッケージを表示
  (setq real-pkgs (mapcar 'car package-alist)) ; インストールされているパッケージのリストを取得
  (message "Installed packages - %s" (reverse real-pkgs)) ; インストールされているパッケージを表示
  ;; アップデートされているパッケージを表示
  (package-menu--refresh real-pkgs)
  (dolist (update-pkg (package-menu--find-upgrades))
    (message "Info: Package %s is updated. Version %s is available."
             (car update-pkg)
             (package-desc-version (cdr update-pkg))))
  ;; インストールされているパッケージで、指定したパッケージ以外のものがあれば表示
  (dolist (pkg pkgs)
    (setq real-pkgs (delete pkg real-pkgs)))
  (when real-pkgs
    (message "Info: Unexpected installed packages %s"  (reverse real-pkgs))))

;;
;; ライブラリの読み込み
;;
(dolist                                 ; 読み込むライブラリー
    (feat
     '(
       ;; built-in libraries
       server
       uniquify
       ;; ~/.emacs.d/site-lisp
       auto-elc-mode                    ; .elファイルの自動コンパイル
       buffer-window-plus               ; バッファとウィンドウの操作関数を追加
       count-japanese                   ; 日本語の文字数をカウント
       ediff-vc-plus                    ; Ediffの関数を追加
       exopen                           ; 外部プログラムでファイルを開く
       not-kill-but-bury-buffer         ; *scratch* と *Messages* のバッファを削除しない
       scroll-one-line                  ; 1行スクロール
       temp-buffer                      ; 一時バッファの作成
       ucs-normalize                    ; 濁点を直す
       window-control                   ; ウィンドウとフレームのサイズを調整
       ))
  (my-init-require feat))

;; autoloadの設定
(let (funcs)
  (dolist                               ; autoloadする関数
      (funcfile
       '(
         (R-mode "ess-site" "Emacs Speaks Statistics mode")
         (bison-mode "bison-mode" "Major mode for editing bison/yacc files")
         (eukleides-mode "eukleides" "Major mode for editing Eukleides files")
         (flex-mode "flex-mode" "Major mode for editing flex files")
         (mpv-ts-mode "mpv-ts" "transcription using mpv")
         (review-mode "review-mode" "Re:VIEW text editing mode")
         (rubydb "rubydb3x" "ruby debug")
         (svg-clock "svg-clock" "Start/stop svg-clock")
         (mediawiki-mode "mediawiki" "Major mode for editing Mediawiki articles.")
         (ucs-normalize-NFC-buffer "ucs-normalize-plus" "Normalize current buffer by the Unicode NFC.")
         ))
    (let ((func (car funcfile)) (file (nth 1 funcfile)) (doc (nth 2 funcfile)))
      (if (not (locate-library file))
          (message "Warning: library file `%s' autoloaded from `%s' is not found." file func))
      (when (autoload func file doc 1)
        (add-to-list 'funcs func))))
  (message "Autoload functions set in init.el: %s" (reverse funcs)))

;;
;; 文字コードの設定
;;

;; 日本語環境
(set-language-environment 'Japanese)
;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8)

;; マイナーモードの設定
(dolist                                 ; マイナーモード
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
       (transient-mark-mode 1)    ; リージョンをハイライト
       (whitespace-mode 1)        ; 空白を強調表示
       ;; 無効にするマイナーモード
       (blink-cursor-mode 0)        ; カーソルは点滅しない
       (electric-indent-mode 0) ; 改行時の自動インデントを無効に（Emacs24から、初期値が有効）
       (menu-bar-mode 0)          ; メニューバーを表示しない
       (tool-bar-mode 0)          ; ツールバーを表示しない
       ))
  (if (not (fboundp (car mode)))
      (message "Warning: In setting minor mode, function %s is void." (car mode))
    (eval mode)))

;; メッセージダイアログボックスは使わない
(defalias 'message-box 'message)

;; カスタム変数デフォルト値の設定
(dolist                                 ; カスタムデフォルト値
    (varval
     '(
       (indent-tabs-mode nil)           ; タブをスペースに展開
       (tab-width 4)                    ; タブ幅は4
       ))
    (custom-set-default (car varval) (cadr varval)))

;; 変数デフォルト値の設定
(setq-default indent-line-function 'indent-to-left-margin) ; インデント用のファンクション

;; カスタム変数の設定
(custom-set-variables
 '(case-replace nil)           ; 置換時に大文字小文字を区別しない
 '(delete-old-versions 1) ; 古いバックアップファイルを自動的に削除する
 '(disabled-command-function nil) ; すべてのコマンドの使用制限を解除する
 '(enable-recursive-minibuffers 1)      ; 再帰的にミニバッファを使う
 '(eval-expression-print-length nil)    ; evalした結果を全部表示する
 '(history-delete-duplicates 1)   ; 重複する履歴は削除
 '(history-length t)              ; 履歴の数を無制限に
 '(inhibit-startup-screen 1)            ; 起動時の画面を表示しない
 '(initial-scratch-message nil)   ; *scratch* にメッセージを表示しない
 '(kill-whole-line 1)          ; kill-lineのとき、改行も含めて切り取り
 '(make-backup-files 1)     ; バックアップファイルを作成する
 '(next-line-add-newlines nil) ; ファイル末尾での改行で、end of bufferエラーが発生しないように
 '(scroll-conservatively 1) ; 画面最下部で下向き、画面最上部で上向きにスクロールするとき、1行ずつスクロール
 '(truncate-lines nil)         ; 継続行を表示しない
 '(truncate-partial-width-windows nil)  ; 行を切り捨てない
 '(use-dialog-box nil)                  ; ダイアログボックスは使わない
 '(user-mail-address "j8takagi@nifty.com") ; ChangeLogなどの設定
 '(version-control 1)   ; バックアップファイルにバージョン番号を付ける
 '(visible-bell 1) ; エラー時、音が鳴るのではなく、画面が点滅するように
 '(yank-pop-change-selection 1)         ; yank-popを有効にする
)

;; フレームの設定
(unless (equal window-system nil)
  (dolist                               ; フレームパラメーター
      (val
       '(
         (foreground-color . "black")
         (background-color . "gray99")
         (cursor-color . "DarkOliveGreen")
         (cursor-type . box)
         ))
    (add-to-list 'default-frame-alist val)))

;; ファイル名の補完入力の対象外にする拡張子。diredで淡色表示される
(dolist                                 ; 補完入力対象外の拡張子
    (ext
     '(
       ".bak" ".d" ".fls" ".log" ".dvi" ".xbb" ".out" ".prev" "_prev"
        ".idx" ".ind" ".ilg" ".tmp" ".synctex.gz" ".dplg" ".dslg"
       ".dSYM/" ".DS_Store"
       ))
  (add-to-list 'completion-ignored-extensions ext))

;; バックアップファイルの保存先
(dolist                                 ; バックアップ保存先
     (ptndir
      '(
        ("." . "~/backup")
        ))
   (let ((dir (expand-file-name (cdr ptndir))))
     (if (not (file-directory-p dir))
         (message "Warning: Backup directory `%s' is NOT exist." dir)
       (setcdr ptndir dir)
       (add-to-list 'backup-directory-alist ptndir))))

;; Infoの設定
(eval-after-load 'info
  '(progn
     (custom-set-variables
      '(Info-directory-list (reverse Info-directory-list))
      )
     (dolist                            ; Infoのパス
       (path
        '(
          "/usr/local/share/info/ja"
          "~/share/info/ja"
          "~/share/info"
          ))
     (let ((fullpath (expand-file-name path)))
       (if (not (car (file-attributes fullpath)))
           (message "Warning: Info path `%s' is not exist or not directory." path)
         (add-to-list 'Info-directory-list fullpath 1))))))

;; dired
(eval-after-load 'dired
  '(progn
     (custom-set-variables
      '(dired-recursive-copies 'always)  ; 確認なしにディレクトリーを再帰的にコピーする
      )
     (dolist
         (feat                          ; dired用に読み込むライブラリー
          '(
            dired-x                     ; diredの拡張機能
            image-dired                 ; サムネイル表示
            sorter                      ; ソート
            wdired                      ; ファイル名編集
          ))
     (my-init-require feat))))

;; view-modeの設定
(eval-after-load 'view
  '(progn
     ;; read-onlyファイルをview-modeで開く
     (my-init-require 'init-view-mode)
     (custom-set-variables '(view-read-only 1))
     (my-init-require 'view-mode-vi-bindings) ;; view-modeでviのキーバインド
     (with-current-buffer "*Messages*" (view-mode)))) ;; *Messages* バッファーを view-mode に

;; バッファ全体の濁点分離を直す
(eval-after-load 'ucs-normalize
  '(my-init-require 'ucs-normalize-plus))

;; lisp-mode
(defun my-init-indent-lisp-indent-line () ; インデントの設定
  (setq indent-line-function 'lisp-indent-line))

(dolist
    (func
     '(
       my-init-indent-lisp-indent-line
       turn-on-auto-elc
       ))
  (add-hook 'emacs-lisp-mode-hook func))

;; Ediff
(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally))

;; uniquify
(custom-set-variables
 '(uniquify-buffer-name-style 'post-forward-angle-brackets)
 '(uniquify-ignore-buffers-re "*[^*]+*"))

;; *compilation*バッファをスクロールして表示
(eval-after-load 'compile
  '(custom-set-variables '(compilation-scroll-output 'first-error)))

;; autoinsert
;; 参考 http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/autoinsert.html
(custom-set-variables
 '(auto-insert-directory "~/.emacs.d/insert/")
 '(auto-insert-query nil)
 '(auto-insert-alist nil))

(my-init-require 'skeleton-file-name)

(my-init-require 'skeleton-pair-japanese)

(dolist                            ; モードごとのautoinsert設定
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
      `(my-init-require ',skel))))

;; emacsclient
(eval-after-load 'server
  '(unless (server-running-p)
     (server-start)))

;; ChangeLog
(eval-after-load 'add-log
  '(custom-set-variables '(change-log-default-name "~/ChangeLog")))

;; vc-follow-linkを無効にする
;; 参考 http://d.hatena.ne.jp/a_bicky/20140607/1402131090
(eval-after-load 'vc-hooks
  '(custom-set-variables '(vc-follow-symlinks nil)))

;; whitespace
(eval-after-load 'whitespace
  '(my-init-require 'init-whitespace))

;; shell-mode
(eval-after-load 'shell
  '(progn
     (custom-set-variables              ; プロンプトの表示設定
      '(shell-prompt-pattern
        "[~/][~/A-Za-z0-9_^$!#%&{}`'.,:()-]* \\[[0-9:]+\\] *$ "))
     (my-init-require 'set-process-query-on-exit)))

;;; CC-Mode
(eval-after-load 'cc-mode
  '(progn
     (custom-set-variables '(c-default-style "k&r"))
     (custom-set-variables '(c-basic-offset 4))
     (defun my-init-cc-ggtags-mode-turnon ()
       (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
         (ggtags-mode 1)))
     (defun my-init-require-gnu-mp ()
       (when (derived-mode-p 'c-mode 'c++-mode)
         (my-init-require 'gnu-mp)))
     (dolist                            ; c-mode-common-hookに追加する関数
         (func
          '(
            my-init-cc-ggtags-mode-turnon
            my-init-require-gnu-mp
            ))
       (add-hook 'c-mode-common-hook func))))

;; tex-mode
(eval-after-load 'tex-mode
  '(add-hook 'latex-mode-hook 'turn-on-reftex))

;; web-mode
(eval-after-load 'web-mode
  '(my-init-require 'init-web-mode))

;; ess-site > R
(eval-after-load 'ess-site
  '(custom-set-variables '(ess-ask-for-ess-directory nil)))

;; bison-mode
(eval-after-load 'bison-mode
  '(custom-set-variables
      '(bison-decl-token-column 0)
      '(bison-rule-enumeration-column 8)))

(defun kill-local-compile-command ()
  (kill-local-variable 'compile-command))

;; graphviz-dot-mode
(eval-after-load 'graphviz-dot-mode
  '(add-hook 'graphviz-dot-mode-hook 'kill-local-compile-command))

;; magit
(eval-after-load 'magit
  '(custom-set-variables '(magit-status-buffer-switch-function 'switch-to-buffer)))

;; mew
(custom-set-variables '(read-mail-command 'mew))
(define-mail-user-agent
  'mew-user-agent
  'mew-user-agent-compose
  'mew-draft-send-message
  'mew-draft-kill
  'mew-send-hook)

;; ruby-mode
(eval-after-load 'ruby-mode
  '(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)))

;;
;; ファイルの自動判定
;;

;; magic-mode-alist
(dolist                                 ; magic-mode-alistのパターン
    (alist
     '(
       ("<![Dd][Oo][Cc][Tt][Yy][Pp][Ee] [Hh][Tt][Mm][Ll]" . web-mode)
       ("<\\?xml " . nxml-mode)
       ))
  (let ((mode (cdr alist)))
    (if (not (fboundp mode))
        (message "Warning: In setting magic-mode-alist, function `%s' is void." mode)
      (add-to-list 'magic-mode-alist alist 1))))

;; auto-mode-alist
;; 既存のモード設定を上書きする
(dolist                                 ; auto-mode-alistで上書きするモード
    (mode
     '(
       (makefile-gmake-mode makefile-bsdmake-mode)
       (web-mode html-mode)
       ))
  (let ((newmode (car mode)) (oldmode (cadr mode)) alist)
    (if (or (not (fboundp oldmode)) (not (fboundp newmode)))
        (dolist (md mode)
          (when (not (fboundp md))
            (message "Warning: In setting auto-mode-alist, function `%s' is void." md)))
      (while (setq alist (rassq oldmode auto-mode-alist))
        (setcdr alist newmode)))))

;; 新しいモード設定を追加する
(dolist                                 ; auto-mode-alistに追加するモード
    (alist
     '(
       ("[Mm]akefile\\.[a-zA-Z0-9]+\\'" . makefile-gmake-mode)
       ("\\.[rR]\\'" . R-mode)
       ("\\.casl?\\'" . asm-mode)
       ("\\.d\\'". makefile-gmake-mode)
       ("\\.euk\\'" . eukleides-mode)
       ("\\.gp\\'" . gnuplot-mode)
       ("\\.gv\\'" . graphviz-dot-mode)
       ("\\.ll?\\'" . flex-mode)
       ("\\.md\\'" . markdown-mode)
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
    (if (not (fboundp mode))
        (message "Warning: In setting auto-mode-alist, function `%s' is void." mode)
      (add-to-list 'auto-mode-alist alist))))

;;
;; キーバインド
;;

;; global-key
(dolist                                 ; グローバルのキーバインド
    (mapkeys
     '(
       ("<M-down>" windmove-down)
       ("<M-f9>" gnuplot-make-buffer)
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
       ("C-c l" magit-log-buffer-file)
       ("C-c t" switch-to-temp-buffer)
       ("C-c w t" whitespace-toggle-options)
       ("C-c w w" whitespace-mode)
       ("C-h TAB" info-lookup-symbol)
       ("C-j" newline)
       ("C-x '" just-one-space)
       ("C-x 4 C-k" delete-kill-next-window-buffer)
       ("C-x 4 C-s" scratch-other-window)
       ("C-x 4 K" delete-kill-next-window-buffer)
       ("C-x 4 k" kill-next-window-buffer)
       ("C-x 4 m" message-other-window)
       ("C-x 4 q" quit-next-window)
       ("C-x 4 s" split-shell-current-directory)
       ("C-x 4 |" toggle-split-next-window)
       ("C-x 4 ~" swap-buffer-next-window)
       ("C-x 5 C-s" new-frame-scratch)
       ("C-x 5 m" new-frame-messages)
       ("C-x 5 s" new-frame-shell-current-directory)
       ("C-x C-M-b" electric-buffer-list)
       ("C-x C-M-f" exopen-find-file)
       ("C-x C-M-k" delete-kill-current-next-window-buffer)
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
    (if (not (fboundp func))
        (message "Warning: In setting keybind, function `%s' is void." func)
      (global-set-key (kbd key) func))))

;; ffap（find file at point）のキーバインド
(ffap-bindings)

;; 無効にするキーバインド
(dolist                                 ; グローバルで無効にするキー
    (key
     '(
       "C-x C-d"                         ; ffap-list-directory を無効に
       "C-x 4 0"                         ; kill-buffer-and-window を無効に
       "M-`"                             ; tmm-menubar を無効に
       ))
  (global-unset-key (kbd key)))

;; モードごとのキーバインドを設定
;; リストの形式は、(mode-library mode-map-name ((key1 function1) (key2 function2)))
(dolist                                 ; モードごとのキーバインド
    (list
     '(
       ("text-mode" nil text-mode-map
        (
         ("C-M-i" dabbrev-expand) ; ispell 起動を無効にし、dabbrev-expand を設定
         ))
       ("dired" nil dired-mode-map
        (
         ("C-c ." dired-exopen-current-directory)
         ("C-c e" ediff-revision)
         ("C-c i" image-dired)
         ("C-c r" dired-exopen-file)
         ("C-c w" wdired-change-to-wdired-mode)
         ("E" dired-ediff-vc-latest-current)
         ("r" dired-exopen-file)
         ("s" dired-toggle-sort)
         ))
       ("tex-mode" nil latex-mode-map
        (
         ("<M-return>" latex-insert-item) ; latex-insert-itemを再設定
         ("C-c p p" exopen-buffer-pdffile)
         ("C-c p d" exopen-buffer-dvifile)
         ("C-c C-c" comment-region)     ; tex-compileを無効にし、comment-region を設定
         ))
       ("lisp-mode" nil lisp-mode-shared-map
        (
         ;; ("<M-return>" noexist)      ; デバッグ用
         ("<M-return>" completion-at-point)
         ("C-c e" eval-buffer)
         ))
       ("mediawiki" nil mediawiki-mode-map
        (
         ("C-x C-s" save-buffer)
         ))
       ))
  (let ((lib (car list)) (hook (nth 1 list))
         (modemap (nth 2 list)) (mapkeys (nth 3 list)))
    (eval-after-load lib
      (cond
       ((null hook)
        `(dolist (map ',mapkeys)
           (let ((key (car map)) (func (nth 1 map)))
             (if (not (fboundp func))
                 (message "Warning: In setting %s keybind, function `%s' is void."
                          ,modemap func)
               (define-key ,modemap (kbd key) func)))))
       (t
        (let* ((modemap-name (symbol-name modemap))
              (func-init-keybind (read (concat "my-init-" modemap-name "-keybind"))))
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
          `(add-hook ',hook ',func-init-keybind)))))))

;;
;; システムごとの初期化ファイルの設定
;;
(defvar system-name-simple
  (replace-regexp-in-string "\\..*\\'" "" (system-name))
  "The simple host name of the machine Emacs is running on, which is without domain information.")

(dolist                                 ; システムごとの初期化ライブラリー
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

;; Emacs変数exec-pathに、環境変数PATHの内容を設定
(when (member window-system '(x mac ns))
  (setq exec-path nil)
  (dolist
      (dir (parse-colon-path (getenv "PATH")))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir t))))

;; Emacs開始にかかった時間をメッセージに表示
(defun my-init-message-startup-time ()
  (message "Duration of the Emacs initialization - %s" (emacs-init-time)))

(dolist
    (hookfunc                           ; フックに設定するファンクション
     '(
       (after-init-hook session-initialize)
       (after-init-hook my-init-message-startup-time)
       (find-file-hook auto-insert)
       (kill-buffer-query-functions not-kill-but-bury-buffer)
       ))
  (let ((hook (car hookfunc)) (func (cadr hookfunc)))
    (cond
     ((not (boundp hook)) (message "hook `%s' is void." hook))
     ((not (fboundp func)) (message "function `%s' is void." func))
     (t
      (add-hook hook func)))))

(message "End of loading init.el.")
