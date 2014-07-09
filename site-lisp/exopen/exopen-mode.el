;;; exopen-mode.el

;; Copyright (C) 2013  j8takagi

;; Author: j8takagi <j8takagi@nifty.com>
;; Keywords: Emacs external program 外部プログラム

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; Emacsからの外部プログラム呼び出しを支援するマイナーモード。

;; ■動作環境
;; Linux、Mac OS X、WindowsのいずれかのWindowシステム上

;; ■インストール方法
;; 1. LOAD-PATHで指定されているディレクトリにこのファイルをコピーする
;;
;; 2. Emacsの設定ファイル（~/.emacs.d/init.el など）に以下の行を追加する
;;
;; (require 'exopen-mode)

;; Windowシステム上でEmacsが動作している場合、exopen-mode は自動的に有効になります。
;;
;; ■使い方
;; exopen-modeが有効な場合、次の関数を使うことができます。
;;
;;   exopen-file (file) 外部ファイルを呼び出す
;;   標準で、C-x C-M-fにバインド
;; ■hook

;; マイナーモードの定義
(define-minor-mode exopen-mode
  "Open file by external program."
  (display-graphic-p)                           ; Windowシステムかどうかの判定
  nil                                           ; モード行に何も表示しない
  `((,(kbd "C-x C-M-f") . exopen-find-file)))    ; キーバインド

;; exopen-std-cmd: OSやWindowで設定された関連付けをもとに
;; ファイルを開くプログラムコマンドとオプション
(defvar exopen-std-cmd nil)
(defvar exopen-std-cmdargs nil)

;; Window別にexopen-std-cmdを設定する
(setq exopen-std-cmd
      (cond
       ((eq window-system 'x) "xdg-open")
       ((eq window-system 'ns) "open")
       ((eq window-system 'w32) "cmd.exe")))

;; Window別にexopen-std-cmdargsを設定する
(setq exopen-std-cmdargs
      (cond
       ((eq window-system 'w32) "/c start")))

;; exopen-modeでの拡張子とプログラムの関連付けリスト
(defvar exopen-suffix-cmd nil)

;;; ファイルを外部プログラムでオープン
;;; exopen-std-cmdで指定されたプログラムを使用
(defun exopen-file (file)
  "open file in external program"
  (let ((process-connection-type nil) (cmd) (cmdargs) (cmdstr))
    (if exopen-suffix-cmd
        (setq cmd (cdr(assoc (file-name-extension file 1) exopen-suffix-cmd))))
    (unless cmd
      (setq cmd exopen-std-cmd)
      (setq cmdargs exopen-std-cmdargs))
    (setq cmdstr (concat cmd " " cmdargs " " file))
    (start-process-shell-command "exopen" nil cmdstr)
    (message
     (concat "exopen: " cmdstr " at "
             (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))))

;;; バッファで開いているファイルを外部プログラムでオープン
(defun exopen-buffer-file ()
  "open buffer file in external program"
  (interactive)
  (if buffer-file-name
      (exopen-file buffer-file-name)
    (error "This buffer is not visiting a file")))

;;; 指定したファイルと同名で拡張子が異なるファイルを外部プログラムでオープン
(defun exopen-buffer-file-suffix (suffix)
  "open buffer file of other extension in external program"
  (let (afile)
    (unless buffer-file-name
      (error "This buffer is not visiting a file"))
    (setq afile (concat (file-name-sans-extension (buffer-file-name)) suffix))
    (if (file-exists-p afile)
        (exopen-file afile)
      (error (concat afile ": file not found")))))

;;; 指定したファイルと同名のPDFファイルを外部プログラムでオープン
(defun exopen-buffer-pdffile ()
  "Open PDF file in external program."
  (interactive)
  (exopen-buffer-file-suffix ".pdf"))

;;; バッファファイルと同名のDVIファイルを外部プログラムでオープン
(defun exopen-buffer-dvifile ()
  "Open DVI file in external program."
  (interactive)
  (exopen-buffer-file-suffix ".dvi"))

;;; バッファファイルと同名のHTMLファイルを外部プログラムでオープン
(defun exopen-buffer-htmlfile ()
  "Open HTML file in external program."
  (interactive)
  (exopen-buffer-file-suffix ".html"))

;;; find-or-bufferがnilの場合はプロンプトで指定したファイル、
;;; nil以外の場合はバッファのファイルを外部プログラムでオープン
(defun exopen-find-file (&optional buffer-file)
  "Open buffer file or find-file in external program."
  (interactive "P")
  (if buffer-file
      (exopen-buffer-file)
    (exopen-file (expand-file-name (read-file-name "Find external open file: " buffer-file-name)))))

;;; dired-modeからファイルやディレクトリーを開く
(require 'dired)

(add-hook
 'dired-mode-hook
 (function
  (lambda ()
    ;; カーソル下のファイルやディレクトリーを外部プログラムで開く
    (defun dired-exopen-file ()
      "Open file mentioned on this line in external program"
      (interactive)
      (exopen-file (dired-get-filename)))

    ;; 現在のディレクトリーを外部プログラムで開く
    (defun dired-exopen-curdir ()
      "Open current directory in external program"
      (interactive)
      (exopen-file (expand-file-name dired-directory)))

    ;; キーバインド
    (define-key dired-mode-map "r" 'dired-exopen-file)
    (define-key dired-mode-map "\C-cr" 'dired-exopen-file)
    (define-key dired-mode-map "\C-c." 'dired-exopen-curdir))))

(provide 'exopen-mode)
;;; exopen-mode.el ends here
