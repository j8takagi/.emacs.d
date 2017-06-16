;;;-*-Emacs-Lisp-*-
(unless noninteractive
  (setq inhibit-message 1))

(message "Start of loading %s at %s." load-file-name (format-time-string "%Y/%m/%d %T"))

(message (emacs-version))

;; user-emacs-directory(~/.emacs.d)のサブディレクトリーをload-pathに追加
(unless noninteractive
  (let ((default-directory (expand-file-name user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'listify)

;;;
;;; パッケージ
;;;
;; パッケージ初期化
(package-initialize)

(listify-packages-add-archives
 '("melpa-stable" "http://stable.melpa.org/packages/")
 )

;; インストールを確認するパッケージ
(listify-packages-check
 'csv-mode
 'ess
 'ggtags
 'gitignore-mode
 'gnuplot
 'graphviz-dot-mode
 'inf-ruby
 'magit
 'markdown-mode
 'mediawiki
 'mew
 'pandoc
 'sokoban
 'web-mode
 'xbm-life
 'xpm
 )

;;
;; ライブラリの読み込み
;;

;; require
(listify-requires
 ;; built-in libraries
 'autoinsert
 'ediff
 'server
 'whitespace
 ;; ~/.emacs.d/site-lisp
 'auto-elc-mode                    ; .elファイルの自動コンパイル
 'buffer-window-plus               ; バッファとウィンドウの操作関数
 'count-japanese                   ; 日本語の文字数をカウント
 'ediff-vc-plus                    ; Ediffの追加関数
 'exopen                           ; 外部プログラムでファイルを開く
 'ffap-plus
 'ime-cursor                       ; IMEをオンにしたときにカーソルの色を変える
 'japanese-plus                    ; 全角半角変換
 'list-fontfamilies-display        ; フォントファミリー一覧作成コマンド
 'list-fonts-display               ; フォント一覧作成コマンド
 'not-kill-but-bury-buffer         ; *scratch* と *Messages* のバッファを削除しない
 'scroll-one-line                  ; 1行スクロール
 'session
 'temp-buffer                      ; 一時バッファの作成
 'window-control                   ; ウィンドウとフレームのサイズを調整
 'xlfd-at                          ; フォント情報の表示
 ;; ~/.emacs.d/insert
 'skeleton-file-name               ; skeletonにより、プロンプトで補完入力したファイル名を挿入
 'skeleton-pair-japanese           ; 日本語の括弧についてのskeleton-pair設定
 )

;; autoload
(listify-autoloads-set
 '(bison-mode "bison-mode" "Major mode for editing bison/yacc files")
 '(crontab-mode "crontab-mode" "Major mode for editing crontab files")
 '(ert-mode "ert-mode" "Major mode for editing ERT files")
 '(eukleides-mode "eukleides" "Major mode for editing Eukleides files")
 '(flex-mode "flex-mode" "Major mode for editing flex files")
 '(mediawiki-mode "mediawiki" "Major mode for editing Mediawiki articles")
 '(mpv-ts-mode "mpv-ts-mode" "Major mode for editing transcription using mpv")
 '(review-mode "review-mode" "Re:VIEW text editing mode")
 '(rubydb "rubydb3x" "ruby debug")
 '(svg-clock "svg-clock" "Start/stop svg-clock")
 '(tsv-mode "tsv-mode" "Major mode for TSV files")
 '(ucs-normalize-NFC-buffer "ucs-normalize-plus" "Normalize current buffer by the Unicode NFC")
 )

;;
;; 文字コード
;;

;; 日本語環境
(set-language-environment 'Japanese)

;; 文字コードのデフォルトはUTF-8
(prefer-coding-system 'utf-8)

;;
;; 一般
;;

; モード
(listify-set-modes
 ;; 有効にするモード
 '(whitespace-mode 1)             ; 空白を強調表示
 )

; 変数
(put 'disabled-command-function 'custom-type '(choice (const nil) function))
(listify-set
 '(Info-additional-directory-list ("~/share/info/ja" "~/share/info" )) ; Infoファイルの場所
 '(auto-compression-mode t)         ; 圧縮されたファイルを直接編集する
 '(auto-insert-alist nil)           ; auto-insert-alistの初期化
 '(auto-insert-directory "~/.emacs.d/insert/") ; auto-insertテンプレートディレクトリ
 '(auto-insert-query nil)                      ; auto-insertでユーザーに尋ねない
 '(backup-directory-alist (("." "~/backup")))
 '(blink-cursor-mode nil)                      ; カーソルは点滅しない
 '(case-replace nil)                ; 置換時に大文字小文字を区別しない
 '(column-number-mode t)            ; 列番号を表示
 '(completion-ignored-extensions ; ファイル名の補完入力の対象外にする拡張子。diredで淡色表示される
   (".bak" ".d" ".fls" ".log" ".dvi" ".xbb"
    ".out" ".prev" "_prev" ".idx" ".ind" ".ilg"
    ".tmp" ".synctex.gz" ".dplg" ".dslg" ".dSYM/"
    ".DS_Store" ":com.dropbox.attributes:$DATA"))
 '(custom-file "~/.emacs.d/.emacs-custom.el") ;カスタムの設定値を書き込むファイル
 '(delete-by-moving-to-trash t)      ;  ファイルの削除で、ゴミ箱を使う
 '(delete-old-versions t) ; 古いバックアップファイルを自動的に削除する
 '(disabled-command-function nil) ; すべてのコマンドの使用制限を解除する
 '(display-buffer-alist (("^\\*shell\\*$" (display-buffer-same-window)) ("^\\*magit: .+" (display-buffer-same-window)))) ; バッファの表示方法
 '(electric-indent-mode nil) ; 改行時の自動インデントを無効に（Emacs24から、初期値が有効）
 '(enable-recursive-minibuffers t)      ; 再帰的にミニバッファを使う
 '(eval-expression-print-length nil)    ; evalした結果を全部表示する
 '(global-font-lock-mode t)       ; メジャーモードに合わせた色を付ける
 '(history-delete-duplicates t)   ; 重複する履歴は削除
 '(history-length t)              ; 履歴の数を無制限に
 '(indent-line-function indent-to-left-margin) ; インデント用のファンクション
 '(indent-tabs-mode nil)          ; タブをスペースに展開
 '(inhibit-startup-screen t)      ; 起動時の画面を表示しない
 '(initial-scratch-message nil)   ; *scratch* にメッセージを表示しない
 '(line-number-mode t)            ; 行番号を表示
 '(make-backup-files t)           ; バックアップファイルを作成する
 '(menu-bar-mode nil)             ; メニューバーを表示しない
 '(next-line-add-newlines nil) ; ファイル末尾での改行で、end of bufferエラーが発生しないように
 '(read-mail-command mew)      ; メールを読むときにmewを使う
 '(save-interprogram-paste-before-kill t) ; 他アプリのコピーバッファをkill-ringに保存する
 '(scroll-conservatively 1) ; 画面最下部で下向き、画面最上部で上向きにスクロールするとき、1行ずつスクロール
 '(session-file-name-history-exclude-regexps ("\\.emacs\\.d/" "~$" "COMMIT_EDITMSG")) ; sessionで、file-name-historyから除外するファイル
 '(session-restore-last-point-exclude-regexps ("COMMIT_EDITMSG")) ; session-restore-last-pointから除外するファイル
 '(show-paren-mode t)             ; 括弧の対応を表示
 '(skeleton-end-hook nil)         ; skeletonの挿入後、改行しない
 '(skeleton-end-newline nil)      ; skeletonの挿入後、改行しない
 '(skeleton-pair t)              ; skeleton-pairにより括弧挿入を自動化
 '(tab-width 4)                  ; タブ幅は4
 '(tool-bar-mode nil)            ; ツールバーを表示しない
 '(transient-mark-mode t)        ; リージョンをハイライト
 '(truncate-lines nil)           ; 継続行を表示しない
 '(truncate-partial-width-windows nil)  ; 行を切り捨てない
 '(use-dialog-box nil)                  ; ダイアログボックスは使わない
 '(user-mail-address "j8takagi@nifty.com") ; ChangeLogなどで用いるメールアドレスの設定
 '(version-control t)   ; バックアップファイルにバージョン番号を付ける
 '(visible-bell t) ; エラー時、音が鳴るのではなく、画面が点滅するように
 '(yank-excluded-properties t)  ; ヤンクで、テキストプロパティは捨てる
 '(yank-pop-change-selection t) ; yank-popを有効にする
 )

(when window-system
  (listify-set
   '(default-frame-alist                ; デフォルトフレーム
      (
       (foreground-color "black")
       (background-color "gray99")
       (cursor-color "DarkOliveGreen")
       (cursor-type box)
       ))))

; エイリアス
(listify-defaliases
 '(uniq-lines delete-duplicate-lines)  ; uniq-linesを、delete-duplicate-linesの別名に
 '(message-box message)                ; メッセージダイアログボックスは使わない
 )

;; view-modeの設定
(with-eval-after-load 'view
  (listify-requires
   'set-view-mode                     ; read-onlyファイルをview-modeで開く
   'view-mode-vi-bindings             ; view-modeでviのキーバインド
   )
  (listify-set
   '(view-read-only t)
   )
  (set-view-mode-buffers
   "\\*Messages\\*"                     ; *Messages*バッファをview-modeに
   ))

;; *Messages*の警告が目立つように
(font-lock-add-keywords 'messages-buffer-mode
                        '(("^\\(\\(Warning\\|Error\\):?\\) .*" 1 font-lock-warning-face t)))
(when (>= emacs-major-version 25)
    (with-current-buffer "*Messages*" (font-lock-ensure)))

;; uniquify
(with-eval-after-load 'uniquify
  (listify-set
   '(uniquify-buffer-name-style post-forward-angle-brackets)
   '(uniquify-ignore-buffers-re "*[^*]+*")))

;; emacsclient
(with-eval-after-load 'server
  (unless (server-running-p)
     (server-start))
  (listify-set
   '(server-window pop-to-buffer)
   ))

;; compile
(with-eval-after-load 'compile
  (listify-set
   '(compilation-scroll-output first-error) ; *compilation*バッファをスクロールして表示
   ))

;; ChangeLog
(with-eval-after-load 'add-log
  (listify-set
   '(change-log-default-name "~/ChangeLog")
   ))

(with-eval-after-load 'vc-hooks
  (listify-set
   '(vc-follow-symlinks nil)            ; vc-follow-linkを無効にする 参考: https://abicky.net/2014/06/07/175130/
   ))

;; whitespace
(with-eval-after-load 'whitespace
  (listify-requires
   'whitespace-plus
   )
  ;タブ	、全角スペース　、行末の空白    
  (listify-set
   '(whitespace-style nil)
   '(whitespace-style (face tabs spaces trailing))
   '(whitespace-space-regexp "\\(　\\)")
   '(whitespace-trailing-regexp "\\( +$\\)")
   '(whitespace-disabled-major-mode-list
     (
     Custom-mode mew-summary-mode completion-list-mode help-mode
     magit-mode tetris-mode w3m-mode mew-message-mode shell-mode
     ))
   '(after-change-major-mode-hook (set-whitespace-mode))
   '(view-mode-hook (set-whitespace-mode)))
  (custom-set-faces
   '(whitespace-space ((nil ( :box (:line-width 2 :color "orange")))))
   '(whitespace-tab ((nil (:background "white smoke" :box (:line-width 2 :color "navy")))))
   '(whitespace-trailing ((nil (:underline "navy"))))
   ))

;;
;; Ediff
;;
(with-eval-after-load 'ediff
  (listify-set
   '(ediff-before-setup-hook ediff-save-window-configuration)
   '(ediff-quit-hook ediff-restore-window-configuration)
   '(ediff-split-window-function split-window-horizontally)
   '(ediff-suspend-hook ediff-restore-window-configuration)
   '(ediff-window-setup-function ediff-setup-windows-plain)
   ))

;;
;; dired
;;
(with-eval-after-load 'dired
  (listify-requires
   'dired-x                             ; diredの拡張機能
   'image-dired                         ; サムネイル表示
   'sorter                              ; ソート
   'wdired                              ; ファイル名編集
   )
  (listify-set
   '(dired-listing-switches "-alh")     ; lsのオプションにhを追加
   '(dired-recursive-copies always) ; diredでディレクトリーを再帰的にコピーするとき、確認しない
   '(dired-dwim-target t)           ; 対象ディレクトリーの推測
   ))

(with-eval-after-load 'find-dired
  (listify-set '(find-ls-option ("-exec ls -ldh {} +" . "-al")))
  )

;;
;; lisp-mode
;;
(with-eval-after-load 'lisp-mode
  (listify-requires
   'emacs-lisp-skeletons
   )
  (defun listify-indent-lisp-indent-line ()
    (set-variable 'indent-line-function 'lisp-indent-line)) ; インデントの設定
  (listify-set
   '(emacs-lisp-mode-hook (listify-indent-lisp-indent-line turn-on-auto-elc))
   '(auto-insert-alist (("\\.el\\'" emacs-lisp-template)))
   ))

;;
;; Shell-mode
;;
(with-eval-after-load 'shell
  (listify-requires
   'set-process-query-on-exit
   )
  (listify-set
   '(shell-prompt-pattern "[~/][~/A-Za-z0-9_^$!#%&{}`'.,:()-]* \\[[0-9:]+\\] *$ ") ; プロンプトの表示設定
   ))

;;
;; CC-Mode
;;
(with-eval-after-load 'cc-mode
  (listify-requires
   'cc-mode-plus
   'c-skeletons
   'h-skeletons
   )
  (listify-set
   '(c-default-style ((c-mode "k&r")))
   '(c-basic-offset 4)
   '(auto-insert-alist (("\\.h\\'" h-template)))
   '(c-mode-common-hook
     (init-cc-ggtags-mode-on init-cc-require-gnu-mp init-cc-disable-electric-state))
   ))

;;
;; tex-mode
;;
(with-eval-after-load 'tex-mode
  (listify-requires
   'latex-skeletons
   )
  (listify-set
   '(auto-insert-alist ((latex-mode latex-template)))
   '(latex-mode-hook (turn-on-reftex))
   ))

;;
;; web-mode
;;
(with-eval-after-load 'web-mode
  (listify-requires
   'web-skeletons
   )
  (listify-set
   '(web-mode-markup-indent-offset 0)     ; HTMLタグのインデントを0に
   '(web-mode-indent-style 1)            ; 1: text at the beginning of line is not indented
   )
  (listify-set
   '(auto-insert-alist (("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" web-template)))
   )
  (custom-set-faces
   '(web-mode-comment-face ((nil (:foreground "#D9333F"))))
   '(web-mode-doctype-face ((nil (:foreground "#82AE46"))))
   '(web-mode-html-attr-name-face ((nil (:foreground "#C97586"))))
   '(web-mode-html-attr-value-face ((nil (:foreground "#82AE46"))))
   '(web-mode-html-tag-face ((nil (:foreground "#00c0e0" :weight bold))))
   '(web-mode-server-comment-face ((nil (:foreground "#D9333F")))))
  )

;;
;; nxml-mode
;;
(with-eval-after-load 'nxml-mode
  (listify-set
   '(nxml-child-indent 0)
   '(nxml-attribute-indent 0)
   ))

;;
;; ess-site > R
;;
(with-eval-after-load 'ess-site
  (listify-set
   '(ess-ask-for-ess-directory nil)
   ))

;;
;; bison-mode
;;
(with-eval-after-load 'bison-mode
  (listify-set
   '(bison-decl-token-column 0)
   '(bison-rule-enumeration-column 8)
   ))

;;
;; graphviz-dot-mode
;;
(with-eval-after-load 'graphviz-dot-mode
  (defun kill-local-compile-command ()
    (kill-local-variable 'compile-command))
  (listify-requires
   'graphviz-dot-skeletons
   )
  (defvar graphviz-dot-mode-hook nil)
  (listify-set
   '(auto-insert-alist ((graphviz-dot-mode graphviz-dot-template)))
   '(graphviz-dot-mode-hook (kill-local-compile-command))
   ))

;;
;; magit
;;
(defvar with-editor-file-name-history-exclude 1) ; "run-hooks: Symbol’s function definition is void: git-commit-setup-check-buffer" エラー対策

;;
;; mew
;;
(define-mail-user-agent
  'mew-user-agent
  'mew-user-agent-compose
  'mew-draft-send-message
  'mew-draft-kill
  'mew-send-hook
  )

;;
;; mpv-ts-mode
;;
(with-eval-after-load 'mpv-ts-mode
  (listify-set
   '(auto-insert-alist ((mpv-ts-mode "template.ts")) 1 (mpv-ts-mode))))

;;
;; ファイルの自動判定
;;

;; magic-mode-alist
(listify-set
 '(magic-mode-alist
   (
    ("<![Dd][Oo][Cc][Tt][Yy][Pp][Ee] [Hh][Tt][Mm][Ll]" web-mode)
    ("<\\?xml " nxml-mode)
    )))

;; auto-mode-alistで、既存のモード設定を上書きする
(listify-overwrite-auto-mode-alist
 '(makefile-gmake-mode makefile-bsdmake-mode)
 '(web-mode html-mode)
 )

;; 新しいモード設定を追加する
(listify-set
 '(auto-mode-alist
   (
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
    ("/crontab\\(\\.[a-zA-Z0-9]+\\)?\\'" crontab-mode)
    ("!.+" conf-mode)
   )))

;;
;; キーバインド
;;

;; global-key
(listify-global-set-keys
 '("<M-down>" windmove-down)
 '("<M-f9>" gnuplot-make-buffer)
 '("<M-left>" windmove-left)
 '("<M-return>" expand-abbrev)
 '("<M-right>" windmove-right)
 '("<M-up>" windmove-up)
 '("C-' h" windmove-left)
 '("C-' j" windmove-down)
 '("C-' k" windmove-up)
 '("C-' l" windmove-right)
 '("C-," scroll-up-one-line)
 '("C-." scroll-down-one-line)
 '("C-M-g" keyboard-escape-quit)
 '("C-`" expand-abbrev)
 '("C-c +" make-directory)
 '("C-c C-c" comment-region)
 '("C-c C-u" uncomment-region)
 '("C-c C-v" view-mode)
 '("C-c c" compile)
 '("C-c l" magit-log-buffer-file)
 '("C-c t" switch-to-temp-buffer)
 '("C-c w t" whitespace-toggle-options)
 '("C-c w w" whitespace-mode)
 '("C-h TAB" info-lookup-symbol)
 '("C-j" newline)
 '("C-x '" just-one-space)
 '("C-x 4 C-k" delete-kill-next-window-buffer)
 '("C-x 4 C-s" scratch-other-window)
 '("C-x 4 K" delete-kill-next-window-buffer)
 '("C-x 4 k" kill-next-window-buffer)
 '("C-x 4 m" message-other-window)
 '("C-x 4 q" quit-next-window)
 '("C-x 4 s" split-shell-current-directory)
 '("C-x 4 |" toggle-split-next-window)
 '("C-x 4 ~" swap-buffer-next-window)
 '("C-x 5 C-s" new-frame-scratch)
 '("C-x 5 m" new-frame-messages)
 '("C-x 5 s" new-frame-shell-current-directory)
 '("C-x C-M-b" electric-buffer-list)
 '("C-x C-M-f" exopen-find-file)
 '("C-x C-M-k" delete-kill-current-next-window-buffer)
 '("C-x E" ediff-redisplay-current-frame)
 '("C-x K" kill-buffer-and-window)
 '("C-x RET u" ucs-normalize-NFC-buffer)
 '("C-x g" magit-status)
 '("C-x m" man)
 '("C-x p" call-last-kbd-macro)
 '("C-x q" bury-buffer)
 '("C-x v e" ediff-vc-latest-current)
 '("C-x v f" find-file-revision)
 '("M-?" help)
 '("M-[" backward-paragraph)
 '("M-]" forward-paragraph)
 '("M-p" call-last-kbd-macro)
 '("RET" newline-and-indent)
 )

;; ffap（find file at point）のキーバインド
(ffap-bindings)

;; 無効にするキーバインド
(listify-global-unset-keys
 "C-x C-d"                         ; ffap-list-directory を無効に
 "C-x 4 0"                         ; kill-buffer-and-window を無効に
 "M-`"                             ; tmm-menubar を無効に
 )

;; モードごとのキーバインドを設定
;; リストの形式は、(mode-library mode-hook mode-map-name ((key1 function1) (key2 function2)))
(listify-modemap-set-keys
 '(text-mode-map "text-mode" nil
   (
    ("C-M-i" dabbrev-expand) ; ispell 起動を無効にし、dabbrev-expand を設定
    ))
 '(dired-mode-map "dired" nil
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
 '(latex-mode-map "tex-mode" nil
   (
    ("<M-return>" latex-insert-item) ; latex-insert-itemを再設定
    ("C-c p p" exopen-buffer-pdffile)
    ("C-c p d" exopen-buffer-dvifile)
    ("C-c C-c" comment-region)     ; tex-compileを無効にし、comment-region を設定
    ))
 '(lisp-mode-shared-map "lisp-mode" nil
   (
    ;; ("<M-return>" noexist)      ; デバッグ用
    ("<M-return>" completion-at-point)
    ("C-c e" eval-buffer)
    ))
 '(mediawiki-mode-map "mediawiki" nil
   (
    ("C-x C-s" save-buffer)
    ))
 '(ediff-mode-map "ediff" ediff-keymap-setup-hook
   (
    ("Q" my-ediff-quit)
    ))
 '(c-mode-map "cc-mode" c-mode-common-hook
   (
    ("{" skeleton-pair-insert-maybe)
    ("(" skeleton-pair-insert-maybe)
    ("'" skeleton-pair-insert-maybe)
    ))
 '(prog-mode-map "prog-mode" nil
   (
    ("'" skeleton-pair-insert-maybe)
    ))
 '(lisp-mode-shared-map "lisp-mode" nil
   (
    ("'" self-insert-command)
    ))
 '(ert-mode-map "ert-mode" nil
   (
    ("'" skeleton-pair-insert-maybe)
    ))
 '(web-mode-map "web-mode" nil
   (
    ("<" skeleton-pair-insert-maybe)
    ("'" skeleton-pair-insert-maybe)
    ))
 '(nxml-mode-map "nxml-mode" nil
   (
    ("<" skeleton-pair-insert-maybe)
    ("'" skeleton-pair-insert-maybe)
    ))
  '(read-expression-map "simple" eval-expression-minibuffer-setup-hook
    (
     ("<M-return>" completion-at-point) ; 補完入力
     ))
  )

;;
;; システムごとの初期化ファイルの設定
;;
(listify-requires-by-system
 '(system-type gnu/linux init-linux)
 '(system-type darwin init-darwin)
 '(window-system mac init-mac-gui)
 '(window-system x init-x)
 '(window-system w32 init-w32)
 )

;; フックの設定
(listify-set
 '(find-file-hook (auto-insert))
 '(kill-buffer-query-functions (not-kill-but-bury-buffer))
 )

(with-eval-after-load 'session
  (listify-set
   '(after-init-hook (session-initialize message-startup-time))
   '(find-file-hook (session-set-file-name-history))
   '(exopen-file-hook (session-set-file-name-history))
   '(session-before-save-hook
     (session-cleanup-file-name-history-exclude-regexp))
   ))

(setq inhibit-message nil)

(message "End of loading init.el.")
