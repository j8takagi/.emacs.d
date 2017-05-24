;;;-*-Emacs-Lisp-*-

(message "Start of loading init.el at %s." (format-time-string "%Y/%m/%d %T"))

(message (emacs-version nil))

;; load-pathの設定
(let ((default-directory (expand-file-name user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

(require 'my-init)

;;;
;;; パッケージ
;;;

(my-init-require 'package)

(dolist                                 ; パッケージアーカイブ
    (archive
     '(
       ("melpa-stable" "http://stable.melpa.org/packages/")
       ))
  (add-to-list 'package-archives (cons (car archive) (cadr archive))))

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
       ediff
       server
       ;; ~/.emacs.d/site-lisp
       auto-elc-mode                    ; .elファイルの自動コンパイル
       buffer-window-plus               ; バッファとウィンドウの操作関数を追加
       count-japanese                   ; 日本語の文字数をカウント
       ediff-vc-plus                    ; Ediffの追加関数
       exopen                           ; 外部プログラムでファイルを開く
       ime-cursor                       ; IMEをオンにしたときにカーソルの色を変える
       japanese-plus                    ; 全角半角変換
       list-fontfamilies-display        ; フォントファミリー一覧を作成
       not-kill-but-bury-buffer         ; *scratch* と *Messages* のバッファを削除しない
       scroll-one-line                  ; 1行スクロール
       temp-buffer                      ; 一時バッファの作成
       window-control                   ; ウィンドウとフレームのサイズを調整
       xlfd-at                          ; フォント情報の表示
       ))
  (my-init-require feat))

;; autoloadの設定
(let (funcs)
  (dolist                               ; autoloadする関数
      (funcfile
       '(
         (bison-mode "bison-mode" "Major mode for editing bison/yacc files")
         (crontab-mode "crontab-mode" "Major mode for editing crontab files")
         (ert-mode "ert-mode" "Major mode for editing ERT files.")
         (eukleides-mode "eukleides" "Major mode for editing Eukleides files")
         (flex-mode "flex-mode" "Major mode for editing flex files")
         (mediawiki-mode "mediawiki" "Major mode for editing Mediawiki articles.")
         (mpv-ts-mode "mpv-ts-mode" "Major mode for editing transcription using mpv.")
         (review-mode "review-mode" "Re:VIEW text editing mode")
         (rubydb "rubydb3x" "ruby debug")
         (svg-clock "svg-clock" "Start/stop svg-clock")
         (tsv-mode "tsv-mode" "Major mode for TSV files")
         (ucs-normalize-NFC-buffer "ucs-normalize-plus" "Normalize current buffer by the Unicode NFC.")
         ))
    (add-to-list
     'funcs
     (my-init-set-autoload (car funcfile) (nth 1 funcfile) (nth 2 funcfile))))
  (message "Autoload functions set in init.el - %s" (reverse funcs)))

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
       (abbrev-mode 1)            ; Abbrevsを使う
       (whitespace-mode 1)        ; 空白を強調表示
       ))
  (my-init-set-mode mode))

;; メッセージダイアログボックスは使わない
(defalias 'message-box 'message)

;; カスタム設定ファイル
(set-variable 'custom-file "~/.emacs.d/.emacs-custom.el")

;; カスタム変数デフォルト値の設定
(dolist                                 ; カスタムデフォルト値
    (varval
     '(
       (indent-tabs-mode nil)           ; タブをスペースに展開
       (tab-width 4)                    ; タブ幅は4
       ))
    (custom-set-default (car varval) (cadr varval)))

;; 変数デフォルト値の設定
(set-default 'indent-line-function 'indent-to-left-margin) ; インデント用のファンクション

;; カスタム変数の設定
(custom-set-variables
 ;; 有効にするモード
 '(auto-compression-mode 1)             ; 圧縮されたファイルを直接編集する
 '(column-number-mode 1)                ; 列番号を表示
 '(global-font-lock-mode 1)             ; メジャーモードに合わせた色を付ける
 '(line-number-mode 1)                  ; 行番号を表示
 '(recentf-mode 1)                      ; recentf
 '(show-paren-mode 1)                   ; 括弧の対応を表示
 '(tool-bar-mode nil)                   ; ツールバーを表示しない
 '(transient-mark-mode 1)               ; リージョンをハイライト
 ;; 無効にするモード
 '(blink-cursor-mode nil)               ; カーソルは点滅しない
 '(electric-indent-mode nil)            ; 改行時の自動インデントを無効に（Emacs24から、初期値が有効）
 '(menu-bar-mode nil)                   ; メニューバーを表示しない
 ;; モード以外
 '(case-replace nil)                    ; 置換時に大文字小文字を区別しない
 '(delete-old-versions 1)               ; 古いバックアップファイルを自動的に削除する
 '(disabled-command-function nil)       ; すべてのコマンドの使用制限を解除する
 '(enable-recursive-minibuffers 1)      ; 再帰的にミニバッファを使う
 '(eval-expression-print-length nil)    ; evalした結果を全部表示する
 '(history-delete-duplicates 1)         ; 重複する履歴は削除
 '(history-length t)                    ; 履歴の数を無制限に
 '(inhibit-startup-screen 1)            ; 起動時の画面を表示しない
 '(initial-scratch-message nil)         ; *scratch* にメッセージを表示しない
 '(make-backup-files 1)                 ; バックアップファイルを作成する
 '(next-line-add-newlines nil)          ; ファイル末尾での改行で、end of bufferエラーが発生しないように
 '(recentf-auto-cleanup 'never)         ; 存在しないファイルを消さない
 '(recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)) ;; 30秒に一度自動保存
 '(recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.*")) ;; recentfの除外ファイル
 '(recentf-max-saved-items 2000)        ; 2000ファイルまで履歴保存する
 '(recentf-save-file "~/.emacs.d/.recentf") ; recentfの設定ファイル
 '(save-interprogram-paste-before-kill 1) ; 他アプリのコピーバッファをkill-ringに保存する
 '(scroll-conservatively 1)             ; 画面最下部で下向き、画面最上部で上向きにスクロールするとき、1行ずつスクロール
 '(session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|\\.emacs\\.d/\\|~$\\|COMMIT_EDITMSG") ; sessionで、file-name-historyから除外するファイル
 '(truncate-lines nil)                  ; 継続行を表示しない
 '(truncate-partial-width-windows nil)  ; 行を切り捨てない
 '(use-dialog-box nil)                  ; ダイアログボックスは使わない
 '(user-mail-address "j8takagi@nifty.com") ; ChangeLogなどで用いるメールアドレスの設定
 '(version-control 1)                   ; バックアップファイルにバージョン番号を付ける
 '(visible-bell 1)                      ; エラー時、音が鳴るのではなく、画面が点滅するように
 '(yank-pop-change-selection 1)         ; yank-popを有効にする
)

;; uniq-linesを、delete-duplicate-linesの別名として設定
(defalias 'uniq-lines 'delete-duplicate-lines)

;; フレームの設定
(unless (null window-system)
  (dolist                               ; フレームパラメーター
      (fparam
       '(
         (foreground-color "black")
         (background-color "gray99")
         (cursor-color "DarkOliveGreen")
         (cursor-type box)
         ))
    (add-to-list 'default-frame-alist (cons (car fparam) (cadr fparam)))))

;; ファイル名の補完入力の対象外にする拡張子。diredで淡色表示される
(dolist                                 ; 補完入力対象外の拡張子
    (ext
     '(
       ".bak" ".d" ".fls" ".log" ".dvi" ".xbb" ".out" ".prev" "_prev"
        ".idx" ".ind" ".ilg" ".tmp" ".synctex.gz" ".dplg" ".dslg"
       ".dSYM/" ".DS_Store" ":com.dropbox.attributes:$DATA"
       ))
  (add-to-list 'completion-ignored-extensions ext))

;; バックアップファイルの保存先
(dolist                                 ; バックアップ保存先
     (ptndir
      '(
        ("." "~/backup")
        ))
   (let (dir)
     (if (not (file-directory-p (setq dir (expand-file-name (cadr ptndir)))))
         (message "Warning: backup directory `%s' is not exist or not directory." dir)
       (add-to-list 'backup-directory-alist (cons (car ptndir) dir)))))

;; Infoの設定
(with-eval-after-load 'info
  (custom-set-variables
   '(Info-directory-list (reverse Info-directory-list))
   )
  (dolist                               ; Infoのパス
      (path
       '(
         "~/share/info/ja"
         "~/share/info"
         ))
    (let (fullpath)
      (if (not (file-directory-p (setq fullpath (expand-file-name path))))
          (message "Warning: info path `%s' is not exist or not directory." fullpath)
        (add-to-list 'Info-directory-list fullpath 1)))))

;; dired
(with-eval-after-load 'dired
  (custom-set-variables
   '(dired-recursive-copies 'always)  ; diredでディレクトリーを再帰的にコピーするとき、確認しない
   '(dired-dwim-target 1)             ; 対象ディレクトリーの推測
   '(dired-isearch-filenames t)       ; diredでのisearchの対象をファイル名だけに
   )
  (dolist
      (feat                          ; dired用に読み込むライブラリー
       '(
         dired-x                     ; diredの拡張機能
         image-dired                 ; サムネイル表示
         sorter                      ; ソート
         wdired                      ; ファイル名編集
         ))
    (my-init-require feat)))

;; view-modeの設定
(with-eval-after-load 'view
  ;; read-onlyファイルをview-modeで開く
  (my-init-require 'init-view-mode)
  (custom-set-variables '(view-read-only 1))
  (my-init-require 'view-mode-vi-bindings) ;; view-modeでviのキーバインド
  (with-current-buffer "*Messages*" (view-mode))) ;; *Messages* バッファーを view-mode に

;; lisp-mode
(with-eval-after-load 'lisp-mode
  (defun my-init-indent-lisp-indent-line () ; インデントの設定
    (set-variable 'indent-line-function 'lisp-indent-line))
  (dolist
      (func
       '(
         my-init-indent-lisp-indent-line
         turn-on-auto-elc
         ))
    (add-hook 'emacs-lisp-mode-hook func)))

;; Ediff
(with-eval-after-load 'ediff
  (custom-set-variables
   '(ediff-window-setup-function 'ediff-setup-windows-plain)
   '(ediff-split-window-function 'split-window-horizontally)))

;; uniquify
(with-eval-after-load 'uniquify
  (custom-set-variables
   '(uniquify-buffer-name-style 'post-forward-angle-brackets)
   '(uniquify-ignore-buffers-re "*[^*]+*")))

;; *compilation*バッファをスクロールして表示
(with-eval-after-load 'compile
  (custom-set-variables '(compilation-scroll-output 'first-error)))

;; autoinsert
;; 参考 http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/autoinsert.html
(custom-set-variables
 '(auto-insert-directory "~/.emacs.d/insert/")
 '(auto-insert-query nil)
 '(auto-insert-alist nil))

;; skeletonにより、プロンプトで補完入力したファイル名を挿入
(my-init-require 'skeleton-file-name)

;; skeleton-pairにより括弧挿入を自動化
(set-variable 'skeleton-pair 1)

;; skeletonの挿入後、改行しない
(set-variable 'skeleton-end-hook nil)

;; 日本語の括弧についてのskeleton-pair設定
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

(dolist                                 ; モードごとのテンプレート
    (modetemplate
     '(
       ("lisp-mode" "\\.el\\'" 'emacs-lisp-template)
       ("graphviz-dot-mode" 'graphviz-dot-mode 'graphviz-dot-template)
       ("cc-mode" "\\.h\\'" 'h-template)
       ("tex-mode" 'latex-mode 'latex-template)
       ("web-mode" "\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" 'web-template)
       ("mpv-ts-mode" 'mpv-ts-mode "template.ts")
       ))
  (eval-after-load (car modetemplate)
    `(define-auto-insert ,(nth 1 modetemplate) ,(nth 2 modetemplate))))

(dolist                                 ; バッファごとの表示の設定
    (bufptn
     '(
       "^\\*shell\\*$"
       "^\\*magit: .+"
       ))
  (add-to-list 'display-buffer-alist
               `(,bufptn . (display-buffer-same-window))))

;; emacsclient
(with-eval-after-load 'server
  (unless (server-running-p)
     (server-start))
  (custom-set-variables
   '(server-window 'pop-to-buffer)))

;; ChangeLog
(with-eval-after-load 'add-log
  (custom-set-variables
   '(change-log-default-name "~/ChangeLog")))

;; vc-follow-linkを無効にする
;; 参考 http://d.hatena.ne.jp/a_bicky/20140607/1402131090
(with-eval-after-load 'vc-hooks
  (custom-set-variables '(vc-follow-symlinks nil)))

;; whitespace
(with-eval-after-load 'whitespace
  (my-init-require 'init-whitespace))

;; shell-mode
(with-eval-after-load 'shell
  (custom-set-variables              ; プロンプトの表示設定
   '(shell-prompt-pattern "[~/][~/A-Za-z0-9_^$!#%&{}`'.,:()-]* \\[[0-9:]+\\] *$ "))
  (my-init-require 'set-process-query-on-exit))

;;; CC-Mode
(with-eval-after-load 'cc-mode
  (my-init-require 'init-cc-mode))

;; tex-mode
(with-eval-after-load 'tex-mode
  (add-hook 'latex-mode-hook 'turn-on-reftex))

;; web-mode
(with-eval-after-load 'web-mode
  (my-init-require 'init-web-mode))

;; nxml-mode
(with-eval-after-load 'nxml-mode
  (custom-set-variables
   '(nxml-child-indent 0)
   '(nxml-attribute-indent 0)))

;; ess-site > R
(with-eval-after-load 'ess-site
  (custom-set-variables '(ess-ask-for-ess-directory nil)))

;; bison-mode
(with-eval-after-load 'bison-mode
  (custom-set-variables
   '(bison-decl-token-column 0)
   '(bison-rule-enumeration-column 8)))

;; graphviz-dot-mode
(defun kill-local-compile-command ()
  (kill-local-variable 'compile-command))

(with-eval-after-load 'graphviz-dot-mode
  (add-hook 'graphviz-dot-mode-hook 'kill-local-compile-command))

;; magit
(with-eval-after-load 'magit
  '(custom-set-variables
    '(magit-status-buffer-switch-function 'switch-to-buffer)))

;; Magitの「run-hooks: Symbol’s function definition is void: git-commit-setup-check-buffer」エラー対策
(defvar with-editor-file-name-history-exclude 1)

;; mew
(custom-set-variables '(read-mail-command 'mew))
(define-mail-user-agent
  'mew-user-agent
  'mew-user-agent-compose
  'mew-draft-send-message
  'mew-draft-kill
  'mew-send-hook)

;;
;; ファイルの自動判定
;;

;; magic-mode-alist
(dolist                                 ; magic-mode-alistのパターン
    (magic
     '(
       ("<![Dd][Oo][Cc][Tt][Yy][Pp][Ee] [Hh][Tt][Mm][Ll]" web-mode)
       ("<\\?xml " nxml-mode)
       ))
  (let (mode)
    (if (not (fboundp (setq mode (cadr magic))))
        (message "Warning: In setting magic-mode-alist, function `%s' is void." mode)
      (add-to-list 'magic-mode-alist (cons (car magic) mode) 1))))

;; auto-mode-alist
;; 既存のモード設定を上書きする
(dolist                                 ; auto-mode-alistで上書きするモード
    (mode-to-from
     '(
       (makefile-gmake-mode makefile-bsdmake-mode)
       (web-mode html-mode)
       ))
  (my-init-overwrite-auto-mode-alist (car mode-to-from) (cadr mode-to-from)))

;; 新しいモード設定を追加する
(dolist                                 ; auto-mode-alistに追加するモード
    (ptnmode
     '(
       ("[Mm]akefile\\(\\.[a-zA-Z0-9]+\\)?\\'" makefile-gmake-mode)
       ("\\.[rR]\\'" R-mode)
       ("\\.casl?\\'" asm-mode)
       ("\\.tsv\\'" tsv-mode)
       ("\\.d\\'" makefile-gmake-mode)
       ("\\.ert\\'" ert-mode)
       ("\\.euk\\'" eukleides-mode)
       ("\\.gp\\'" gnuplot-mode)
       ("\\.gv\\'" graphviz-dot-mode)
       ("\\.ll?\\'" flex-mode)
       ("\\.md\\'" markdown-mode)
       ("\\.re\\'" review-mode)
       ("\\.svg\\'" nxml-mode)
       ("\\.ts\\'" mpv-ts-mode)
       ("\\.wiki\\'" mediawiki-mode)
       ("\\.xml\\'" nxml-mode)
       ("\\.y?rb\\'" ruby-mode)
       ("\\.yy?\\'" bison-mode)
       ("\\`ja.wikipedia.org/w/index.php" mediawiki-mode)
       ("abbrev_defs" emacs-lisp-mode)
       ("cmd" shell-script-mode)
       ("crontab\\(\\.[a-zA-Z0-9]+\\)?" crontab-mode)
       ("!.+" conf-mode)
       ))
  (let (mode)
    (if (not (fboundp (setq mode (cadr ptnmode))))
        (message "Warning: In setting auto-mode-alist, function `%s' is void." mode)
      (add-to-list 'auto-mode-alist (cons (car ptnmode) mode)))))

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
       ("C-M-g" keyboard-escape-quit)
       ("C-`" expand-abbrev)
       ("C-c +" make-directory)
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
       ("C-x E" ediff-redisplay-current-frame)
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
  (my-init-global-set-key (car mapkeys) (cadr mapkeys)))

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
;; リストの形式は、(mode-library mode-hook mode-map-name ((key1 function1) (key2 function2)))
(dolist                                 ; モードごとのキーバインド
    (modekey
     '(
       (nil nil read-expression-map
        (
         ("<tab>" lisp-complete-symbol)
         ))
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
       ("ediff" ediff-keymap-setup-hook ediff-mode-map
        (
         ("Q" my-ediff-quit)
         ))
       ("cc-mode" c-mode-common-hook c-mode-map
        (
         ("{" skeleton-pair-insert-maybe)
         ("(" skeleton-pair-insert-maybe)
         ("'" skeleton-pair-insert-maybe)
         ))
       (nil nil prog-mode-map
        (
         ("'" skeleton-pair-insert-maybe)
         ))
       (nil nil lisp-mode-shared-map
        (
         ("'" self-insert-command)
         ))
       ("ert-mode" nil ert-mode-map
        (
         ("'" skeleton-pair-insert-maybe)
         ))
       ("web-mode" nil web-mode-map
        (
         ("<" skeleton-pair-insert-maybe)
         ("'" skeleton-pair-insert-maybe)
         ))
       ("nxml-mode" nil nxml-mode-map
        (
         ("<" skeleton-pair-insert-maybe)
         ("'" skeleton-pair-insert-maybe)
         ))
       ))
  (my-init-modemap-set-key (car modekey) (nth 1 modekey) (nth 2 modekey) (nth 3 modekey)))

(font-lock-add-keywords 'messages-buffer-mode
                        '(("^\\(Warning:\\) .*" 1 font-lock-warning-face t)))
(with-current-buffer "*Messages*"
  (font-lock-ensure))

;;
;; システムごとの初期化ファイルの設定
;;
(defvar system-name-simple
  (replace-regexp-in-string "\\..*\\'" "" (system-name))
  "The simple host name of the machine Emacs is running on, which is without domain information.")

(dolist                                 ; システムごとの初期化ライブラリー
     (syslib
     '(
       (system-type gnu/linux init-linux)
       (system-type darwin init-darwin)
       (window-system mac init-mac-gui)
       (window-system x init-x)
       (window-system w32 init-w32)
       ))
  (when (equal (eval (car syslib)) (nth 1 syslib))
    (my-init-require (nth 2 syslib))))

;; Emacs開始にかかった時間をメッセージに表示
(defun my-init-message-startup-time ()
  (message "Duration of the Emacs initialization - %s" (emacs-init-time)))

;; フックの設定
(dolist
    (hookfunc                           ; フックに設定するファンクション
     '(
       (after-init-hook session-initialize)
       (after-init-hook my-init-message-startup-time)
       (find-file-hook auto-insert)
       (kill-buffer-query-functions not-kill-but-bury-buffer)
       ))
  (my-init-set-hook (car hookfunc) (cadr hookfunc)))

(message "End of loading init.el.")
