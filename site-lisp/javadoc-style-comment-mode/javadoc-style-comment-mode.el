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
;;
;; minor mode of insert JavaDoc style comments
;;
;; * Environment
;;
;; * Install
;; 1. copy this file to a directory included in Emacs LOAD-PATH.
;;
;; 2. puts Emacs setting file such as ~/.emacs.d/init.el below.
;;
;;     (require 'javadoc-style-comment-mode)
;;
;; *Usage
;;
;; *Hook

;; minor-mode definition

; 

(defun javadoc-style-comment-insert ()
  "insert JavaDoc style comment skelton"
  (interactive)
  (insert "/**\n *\n */\n")
  (goto-char (- (point) 5))
  (insert " "))

(defun javadoc-style-comment-newline ()
  "insert JavaDoc style comment new line"
  (interactive)
  (insert "\n * "))

(defvar javadoc-style-comment-mode-keymap (make-keymap))

(define-key global-map (kbd "C-;") javadoc-style-comment-mode-keymap)

(define-key javadoc-style-comment-mode-keymap (kbd "C-;") 'javadoc-style-comment-insert)
(define-key javadoc-style-comment-mode-keymap (kbd "RET") 'javadoc-style-comment-newline)

(define-minor-mode javadoc-style-comment-mode ()
  "minor mode of insert JavaDoc style comments"
  t
  nil                                     ; モード行に何も表示しない
  nil)

(provide 'javadoc-style-comment-mode)
;;; javadoc-style-comment-mode.el ends here
