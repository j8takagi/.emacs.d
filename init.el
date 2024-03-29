;;; init.el -*- lexical-binding: t -*-
(unless noninteractive
  (setq inhibit-message 1))

(message "Start of loading %s at %s." load-file-name (format-time-string "%Y/%m/%d %T"))

(defun init-message-startup-time ()
  "Message Duration of the Emacs initialization time."
  (message "Duration of the Emacs initialization - %s" (emacs-init-time)))

(message (emacs-version))

;; user-emacs-directory(~/.emacs.d)のサブディレクトリーをload-pathに追加
(let ((default-directory (expand-file-name user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path))

(require 'listify)

;;
;; パッケージ
;;
(eval-and-compile
  (listify-require 'listify-packages)
)

;; パッケージアーカイブの指定
(listify-packages-add-archives
 '("melpa-stable" "http://stable.melpa.org/packages/")
 )

;; インストールを確認するパッケージ
(listify-packages-check
 'bison-mode
 'csv-mode
 'ess
 'gitignore-mode
 'gnuplot
 'graphviz-dot-mode
 'gtags-mode
 'inf-ruby
 'js2-mode
 'magit
 'markdown-mode
 'mediawiki
 'pandoc
 'sokoban
 'swift-mode
 'web-mode
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
 ;; ~/.emacs.d/site-lisp
 'count-japanese                   ; 日本語の文字数をカウント
 'continue-scratch                 ; *scratch*の永続化
 'daily-log                        ; 毎日のログ
 'exopen                           ; 外部プログラムでファイルを開く
 'file-name-history-add            ; dired-modeで開いたファイルをfile-name-historyに追加
 'fill-region-with-n               ; 1行あたりの文字数を指定してfill-region
 'fontset-set                      ; フォントセットの設定
 'fontsize-set                     ; フォントサイズの設定関数
 'frame-control                    ; フレームの制御
 'japanese-plus                    ; 全角半角変換
 'jaword                           ; 日本語の単語をきちんと扱う
 'list-fontfamilies-display        ; フォントファミリー一覧作成コマンド
 'list-fonts-display               ; フォント一覧作成コマンド
 'bury-always-buffer               ; *scratch* と *Messages* のバッファを削除しない
 'scroll-one-line                  ; 1行スクロール
 'replace-plus                     ; 置換の追加機能
 'savehist-init                    ; 履歴を保存するsavehist-modeの設定
 'view-mode-init                   ; read-onlyファイルをview-modeで開く
 'whitespace-init                  ; whitespace-modeの設定
 'shell-plus                       ; シェルの追加機能
 'symbol-properties                ; シンボルのプロパティ名リスト取得
 'temp-buffer                      ; 一時バッファの作成
 'toggle-skeleton-pair             ; skeleton-pairのトグル
 'url-util-plus                    ; URLに関するユーティリティ
 'vc-plus                          ; vcの追加関数
 'view-mode-vi-bindings            ; view-modeでviのキーバインド
 'window-control                   ; ウィンドウの制御
 'xlfd-at                          ; フォント情報の表示
 ;; ~/.emacs.d/insert
 'skeleton-file-name               ; skeletonにより、プロンプトで補完入力したファイル名を挿入
 'skeleton-pair-japanese           ; 日本語の括弧についてのskeleton-pair設定
 )

;; autoload
(listify-autoloads-set
 '(crontab-mode "crontab-mode" "Major mode for editing crontab files")
 '(ert-mode "ert-mode" "Major mode for editing ERT files")
 '(eukleides-mode "eukleides" "Major mode for editing Eukleides files")
 '(mediawiki-mode "mediawiki" "Major mode for editing Mediawiki articles")
 '(mpv-ts-mode "mpv-ts-mode" "Major mode for editing transcription using mpv")
 '(review-mode "review-mode" "Re:VIEW text editing mode")
 '(rubydb "rubydb3x" "ruby debug")
 '(svg-clock "svg-clock" "Start/stop svg-clock")
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

; マイナーモード
(listify-set-minor-modes
)

;; ローカル変数に
(listify-variable-buffer-local
 'compile-history
 'grep-history
 )

; 変数
(listify-set
 '(auto-insert-alist nil)               ; auto-insert-alistの初期化
 `(auto-insert-directory ,(locate-user-emacs-file "insert")) ; auto-insertテンプレートディレクトリ
 '(auto-insert-query nil)               ; auto-insertでユーザーに尋ねない
 '(backup-directory-alist (("." "~/backup"))) ; バックアップディレクトリ
 '(blink-cursor-mode nil)               ; カーソルは点滅しない
 '(case-replace nil)                    ; 置換時に大文字小文字を区別しない
 '(column-number-mode t)                ; 列番号を表示
 '(completion-ignored-extensions (".bak" ".d" ".fls" ".log" ".dvi" ".xbb" ".out" ".prev" "_prev" ".idx" ".ind" ".ilg" ".tmp" ".synctex.gz" ".dplg" ".dslg" ".dSYM/" ".DS_Store" ":com.dropbox.attributes:$DATA")) ; ファイル名の補完入力の対象外にする拡張子。diredで淡色表示される
 `(custom-file ,(locate-user-emacs-file ".emacs-custom.el")) ; カスタムの設定値を書き込むファイル
 `(default-directory ,(expand-file-name "~")) ; カレントディレクトリ
 '(delete-by-moving-to-trash t)         ; ファイルの削除で、ゴミ箱を使う
 '(delete-old-versions t)               ; 古いバックアップファイルを自動的に削除する
 '(desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\'\\)\\|\\(~[0-9a-f]+~\\'\\)")
 '(desktop-locals-to-save (buffer-undo-list compile-history grep-history)) ; ローカル変数としてdesktopで保存
 '(desktop-save-mode t) ; 終了時の状態を永続的に保存
 '(dired-always-read-filesystem t)      ; ディレクトリ変更を検索前に反映
 '(dired-auto-revert-buffer t)          ; ディレクトリ変更を反映
 '(dired-kill-when-opening-new-dired-buffer t) ; 別のディレクトリを表示した時、バッファを削除
 '(disabled-command-function nil)       ; すべてのコマンドの使用制限を解除する
 '(display-buffer-alist (("^\\*shell\\*$" (display-buffer-same-window)) ("^\\*?magit: .+" (display-buffer-same-window)))) ; バッファの表示方法
 '(electric-indent-mode nil)            ; 改行時の自動インデントを無効に
 '(enable-recursive-minibuffers t)      ; 再帰的にミニバッファを使う
 '(eval-expression-print-length nil)    ; evalした結果を全部表示する
 '(global-auto-revert-mode t)           ; すべてのバッファで、外部のファイル変更を反映
 '(history-delete-duplicates t)         ; 重複する履歴は削除
 '(history-length t)                    ; 履歴の数を無制限に
 '(indent-line-function indent-to-left-margin) ; インデント用の関数
 '(indent-tabs-mode nil)                ; インデントにタブを使わない
 '(inhibit-startup-screen t)            ; 起動時の画面を表示しない
 '(initial-scratch-message nil)         ; *scratch* にメッセージを表示しない
 '(jaword-mode t)                       ; 日本語の単語をきちんと扱う
 '(menu-bar-mode nil)                   ; メニューバーを表示しない
 '(ring-bell-function ignore)           ; エラー時、なにもしない
 '(save-interprogram-paste-before-kill t) ; 他アプリのコピーバッファをkill-ringに保存する
 '(savehist-mode t)                     ; 履歴を永続的に保存
 '(scroll-conservatively 1)             ; 画面最下部で下向き、画面最上部で上向きにスクロールするとき、1行ずつスクロール
; '(shell-plus-ssh-track-mode t)        ; shellでSSH接続時にdefault-directoryを自動設定
 '(tab-width 4)                         ; タブ幅は4
 '(tool-bar-mode nil)                   ; ツールバーを表示しない
 '(truncate-partial-width-windows nil)  ; 行を切り捨てない
 '(use-dialog-box nil)                  ; ダイアログボックスは使わない
 '(user-mail-address "j8takagi@nifty.com") ; ChangeLogなどで用いるメールアドレスの設定
 '(vc-find-revision-no-save 1)             ; vcで作成した履歴ファイルは保存しない
 '(version-control t)                   ; バックアップファイルにバージョン番号を付ける
 '(view-read-only 1)                    ; view-modeで開いたファイルをread-onlyに
 '(yank-excluded-properties t)          ; ヤンクで、テキストプロパティは捨てる
 '(yank-pop-change-selection t)         ; yank-popを有効にする
 )

;; フック
(listify-set-hooks
 '(after-init-hook (init-message-startup-time listify-message-set-variables))
 '(find-file-hook (auto-insert))
 )

; エイリアス
(listify-defaliases
 '(uniq-lines delete-duplicate-lines)  ; uniq-linesを、delete-duplicate-linesの別名に
 '(message-box message)                ; メッセージダイアログボックスは使わない
 '(s2n string-to-number)               ; 置換時の関数名入力省力化
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

;; C言語ソースの場所
(with-eval-after-load 'find-func
  (listify-set-standard-values
   'find-function-C-source-directory
  )
  (listify-set
   `(find-function-C-source-directory
     ,(expand-file-name
       (concat "~/src/emacs-" emacs-version "/src/")))
   ))


;; Infoの設定
(with-eval-after-load 'info
  (listify-set
   '(Info-additional-directory-list ("~/share/info/ja" "~/share/info" )) ; Infoファイルの場所
   ))

;; skeletonの設定
(with-eval-after-load 'skeleton
  (listify-set
   '(skeleton-end-newline nil)      ; skeletonの挿入後、改行しない
   '(skeleton-pair t)               ; skeleton-pairにより括弧挿入を自動化
   )
  )

(with-current-buffer (buffer-name (messages-buffer))
  ;; *Messages*の警告が目立つように
  (font-lock-mode 1)
  (font-lock-ensure)
  (font-lock-add-keywords
   'messages-buffer-mode
   '(("\\`\\(\\(Warning\\|Error\\):?\\)" 1 font-lock-warning-face t)))
  ;; *Messages*バッファをview-modeに
  (view-mode 1))

;; uniquify
(with-eval-after-load 'uniquify
  (listify-set
   '(uniquify-ignore-buffers-re "*[^*]+*")))

;; emacsclient
(when noninteractive
  (eval-and-compile (require 'server)))
(with-eval-after-load 'server
  (listify-custom-initialize-hook
   'server-after-make-frame-hook
   )
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
(when noninteractive
  (eval-and-compile (require 'whitespace-init)))
(with-eval-after-load 'whitespace-init
  ; タブ	、全角スペース　、行末の空白  
  (when (whitespace-init-tabs-spaces-trailing)
    (listify-set-minor-modes '(whitespace-mode 1)))
  (listify-set
   '(whitespace-init-disabled-major-mode-list
     (
     Custom-mode completion-list-mode help-mode
     magit-mode tetris-mode w3m-mode shell-mode
     )))
  (listify-set-hooks
   '(view-mode-hook (whitespace-init-enable-mode))
   '(after-change-major-mode-hook (whitespace-init-enable-mode))
   )
  )

;;
;; Ediff
;;
(with-eval-after-load 'ediff
  (listify-set
   '(ediff-window-setup-function ediff-setup-windows-plain)
   '(ediff-split-window-function split-window-horizontally)
   ))

;;
;; dired
;;
(with-eval-after-load 'dired
  (listify-requires
   'dired-aux                           ; diredの拡張機能
   'dired-x                             ; diredの拡張機能
   'image-dired                         ; サムネイル表示
   'sorter                              ; ソート
   'wdired                              ; ファイル名編集
   )
  (listify-set
   '(dired-dwim-target t)               ; 対象ディレクトリーの推測
   '(dired-listing-switches "-alh")     ; lsのオプションにhを追加
   '(dired-recursive-copies always)     ; diredでディレクトリーを再帰的にコピーするとき、確認しない
   '(dired-recursive-deletes always)    ; diredでディレクトリーを再帰的に削除するとき、確認しない
   )
  (defun init-turn-on-auto-revert-mode ()
    (auto-revert-mode 1)
    (setq-local auto-revert-verbose nil))
  (listify-set-hooks
   '(dired-mode-hook (init-turn-on-auto-revert-mode))
   )
  )

(with-eval-after-load 'dired-aux
  (listify-set
   '(dired-do-revert-buffer t)          ; dired-do操作のあと、diredバッファを更新
   ))

(with-eval-after-load 'find-dired
  (listify-set
   '(find-ls-option ("-exec ls -ldh {} +" . "-alh"))
  ))

;;
;; lisp-mode
;;
(with-eval-after-load 'lisp-mode
  (eval-and-compile
    (listify-requires
     'emacs-lisp-skeletons
     'auto-elc-mode                    ; .elファイルの自動コンパイル
     ))
  (defun init-lisp-indent-line ()
    (set-variable 'indent-line-function 'lisp-indent-line)) ; インデントの設定
  (defun init-turn-on-auto-elc ()
    (auto-elc-mode 1))
  (listify-set
   '(auto-insert-alist (("\\.el\\'" emacs-lisp-template)))
   )
  (listify-set-hooks
   '(emacs-lisp-mode-hook (init-lisp-indent-line init-turn-on-auto-elc))
   )
  )

;;
;; Shell-mode
;;
(with-eval-after-load 'shell
  (listify-custom-initialize-hooks
   'comint-password-function
   )
  (listify-set-hooks
   '(shell-mode-hook (ansi-color-for-comint-mode-on))
   )
  )

;;
;; asm-mode
;;
(with-eval-after-load 'asm-mode
  (defun init-set-tab-width-8 ()
    (interactive)
    (setq tab-width 8))
  (listify-set-hooks
   '(asm-mode-hook
     (init-set-tab-width-8 overwrite-mode))))

;;
;; CC-Mode
;;
(with-eval-after-load 'cc-mode
  (listify-requires
   'c-skeletons
   'h-skeletons
   'gtags
   )
  (listify-set
   '(auto-insert-alist (("\\.h\\'" h-template)))
   '(c-basic-offset 4)
   '(c-default-style ((c-mode "k&r")))
   )
  )

;;
;; tex-mode
;;
(with-eval-after-load 'tex-mode
  (listify-requires
   'latex-skeletons
   )
  (listify-set
   '(auto-insert-alist ((latex-mode latex-template)))
   )
  (listify-set-hooks
   '(latex-mode-hook (turn-on-reftex))
   )
  )

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
(with-eval-after-load 'ess-custom
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
  (defun init-unset-compile-command ()
    (kill-local-variable 'compile-command))
  (listify-requires
   'graphviz-dot-skeletons
   )
  (defvar graphviz-dot-mode-hook nil)
  (listify-set
   '(auto-insert-alist ((graphviz-dot-mode graphviz-dot-template)))
   )
  (listify-set-hooks
   '(graphviz-dot-mode-hook (init-unset-compile-command))
   )
  )

;; markdown-mode
(with-eval-after-load 'markdown-mode
  (listify-set
   '(markdown-command "pandoc -s --self-contained -t html5 -c ~/.pandoc/github.css") ;markdownからHTML作成
   ))

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

;; auto-mode-alistで、既存のメジャーモード設定を上書きする
(listify-update-cdrs
 '(auto-mode-alist ((makefile-gmake-mode makefile-bsdmake-mode) (web-mode mhtml-mode)))
 )

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
    ("\\.s\\'" asm-mode)
    ("\\.svg\\'" nxml-mode)
    ("\\.ts\\'" mpv-ts-mode)
    ("\\.wiki\\'" mediawiki-mode)
    ("\\.xml\\'" nxml-mode)
    ("\\.y?rb\\'" ruby-mode)
    ("\\.yy?\\'" bison-mode)
    ("\\`ja.wikipedia.org/w/index.php" mediawiki-mode)
    ("abbrev_defs" emacs-lisp-mode)
    ("/cmd\\'" shell-script-mode)
    ("/crontab\\(\\.[a-zA-Z0-9]+\\)?\\'" crontab-mode)
    ("!.+" conf-mode)
   )))

;;
;; キーバインド
;;

;; global-key
(listify-global-set-keys
 '("C-' h" windmove-left)
 '("C-' j" windmove-down)
 '("C-' k" windmove-up)
 '("C-' l" windmove-right)
 '("C-," scroll-up-one-line)
 '("C-." scroll-down-one-line)
 '("C-=" toggle-skeleton-pair)
 '("C-M-g" keyboard-escape-quit)
 '("C-\\" (keymap))
 '("C-\\ f" frame-control-resize)
 '("C-\\ m" frame-control-move)
 '("C-\\ w" window-control-resize)
 '("C-`" expand-abbrev)
 '("C-c +" make-directory)
 '("C-c 1" daily-log-open)
 '("C-c C-c" comment-region)
 '("C-c C-u" uncomment-region)
 '("C-c C-v" view-mode)
 '("C-c c" compile)
 '("C-c l" magit-log-buffer-file)
 '("C-c p" call-last-kbd-macro)
 '("C-c t" switch-to-temp-buffer)
 '("C-c w t" whitespace-toggle-options)
 '("C-c w w" whitespace-mode)
 '("C-h TAB" info-lookup-symbol)
 '("C-j" newline)
 '("C-x '" just-one-space)
 '("C-x 4 C-s" window-control-other-shell)
 '("C-x 4 K" window-control-current-other-kill-buffer)
 '("C-x 4 k" window-control-other-kill-buffer)
 '("C-x 4 m" window-control-other-message)
 '("C-x 4 q" window-control-other-quit)
 '("C-x 4 s" window-control-other-scratch)
 '("C-x 4 |" window-control-toggle-horizontal-vertical)
 '("C-x 4 ~" window-control-swap)
 '("C-x 5 -" frame-control-iconify-other-frames)
 '("C-x 5 =" frame-control-visible-all-frames)
 '("C-x 5 C-s" frame-control-open-shell)
 '("C-x 5 m" frame-control-open-messages)
 '("C-x 5 s" frame-control-open-scratch)
 '("C-x C-M-b" electric-buffer-list)
 '("C-x C-M-f" exopen-find-file)
 '("C-x C-M-k" window-control-current-other-kill-buffer)
 '("C-x E" vc-plus-redisplay-current-frame)
 '("C-x K" kill-buffer-and-window)
 '("C-x RET u" ucs-normalize-NFC-buffer)
 '("C-x g" magit-status)
 '("C-x m" man)
 '("C-x q" bury-buffer)
 '("C-x v -" vc-revision-other-window)
 '("C-x v e" vc-plus-ediff)
 '("C-x v f" vc-plus-find-file-revision)
 '("M-<down>" windmove-down)
 '("M-<f9>" gnuplot-make-buffer)
 '("M-<left>" windmove-left)
 '("M-<return>" expand-abbrev)
 '("M-<right>" windmove-right)
 '("M-<up>" windmove-up)
 '("M-?" help)
 '("M-[" backward-paragraph)
 '("M-]" forward-paragraph)
 '("M-p" call-last-kbd-macro)
 '("M-s M-s" isearch-edit-string)
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

;; メジャーモードごとのキーバインド設定
;; リストの形式は、(mode-library mode-hook mode-map-name ((key1 function1) (key2 function2)))
(listify-modemap-set-keys
 '(text-mode-map "text-mode" nil
   (
    ("C-M-i" dabbrev-expand) ; ispell 起動を無効にし、dabbrev-expand を設定
    ))
 '(dired-mode-map "dired" nil
   (
    ("C-c ." exopen-dired-current-directory)
    ("C-c e" ediff-revision)
    ("C-c i" image-dired)
    ("C-c r" exopen-dired-file)
    ("C-c w" wdired-change-to-wdired-mode)
    ("E" vc-plus-dired-ediff)
    ("r" exopen-dired-file)
    ("s" dired-toggle-sort)
    (";" replace-plus-dired-string-files)
    ("'" replace-plus-dired-regexp-files)
    ))
 '(latex-mode-map "tex-mode" nil
   (
    ("M-<return>" latex-insert-item) ; latex-insert-itemを再設定
    ("C-c p p" exopen-buffer-pdffile)
    ("C-c p d" exopen-buffer-dvifile)
    ("C-c C-c" comment-region)     ; tex-compileを無効にし、comment-region を設定
    ))
 '(lisp-mode-shared-map "lisp-mode" nil
   (
    ;; ("M-<return>" noexist)      ; デバッグ用
    ("M-<return>" completion-at-point)
    ("C-c e" listify-eval-buffer)
    ))
 '(mediawiki-mode-map "mediawiki" nil
   (
    ("C-x C-s" save-buffer)
    ))
 '(ediff-mode-map "ediff" ediff-keymap-setup-hook
   (
    ("Q" vc-plus-quit)
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
     ("M-<return>" completion-at-point) ; 補完入力
     ))
  )

;;
;; システムごとの初期化ファイルの設定
;;
(listify-requires-by-system
 '(system-type gnu/linux init-linux)
 '(system-type darwin init-darwin)
 '(window-system ns init-ns-gui)
 '(window-system x init-x)
 '(window-system w32 init-w32)
 )

(setq inhibit-message nil)

(message "End of loading init.el.")
