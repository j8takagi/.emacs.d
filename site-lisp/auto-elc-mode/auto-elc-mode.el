;;; auto-elc-mode.el -*- lexical-binding: t -*-

;; Copyright (C) 2013  j8takagi

;; Author: j8takagi <j8takagi@nifty.com>
;; Keywords: Emacs elisp compile

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

;; Emacs Lispファイルを自動的にコンパイルするマイナーモード。

;; ■動作環境
;; Emacs 29.1で動作を確認

;; ■インストール方法
;; 1. LOAD-PATHで指定されているディレクトリにこのファイルをコピーする
;;
;; 2. Emacsの設定ファイル（~/.emacs.d/init.el など）に以下の行を追加する
;;
;; (require 'auto-elc-mode)


;; マイナーモードの定義
(define-minor-mode auto-elc-mode
"Toggle auto-elc-mode.
With a prefix argument ARG, enable Auto elc mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

byte compile automatically emacs lisp buffer file after saving."

:lighter: " elc"

;;; ファイルをバイトコンパイルする
(defun auto-elc-byte-compile-current-buffer ()
  "byte compile current emacs lisp buffer when file is saved."
  (interactive)
  (when (and auto-elc-mode (equal (file-name-extension (buffer-file-name)) "el"))
    (byte-compile-file buffer-file-name)))

(if auto-elc-mode
    (add-hook 'after-save-hook 'auto-elc-byte-compile-current-buffer nil 'local)
  (remove-hook 'after-save-hook 'auto-elc-byte-compile-current-buffer 'local)))

(provide 'auto-elc-mode)
;;; auto-elc-mode.el ends here
