;;; window-control.el -*- lexical-binding: t -*-

;; Copyright (C) 2014, 2017, 2023  j8takagi

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

;; Control Emcas frame and window.

;; ■動作環境
;; Emacs 25.2で動作を確認

;; ■インストール方法
;; 1. LOAD-PATHで指定されているディレクトリにこのファイルをコピーする
;;
;; 2. Emacsの設定ファイル（~/.emacs.d/init.el など）に以下の行を追加する
;;
;; (require 'window-control)

;; ■使い方

;; ■hook

;; window-control
;;
;; from: window-resizer - http://d.hatena.ne.jp/khiker/20100119/window_resize


(defgroup window-control nil
  "Windows and frame control utilities."
  :prefix "window-control-"
  :version "25.2"
  :group 'windows
  :group 'convenience
  )

(defvar window-control-resize-hook)

;; 隣のウィンドウのバッファを削除する。
;; 引数kill-buffer-only-pがnilの場合、ウィンドウも閉じる
(defun window-control-other-kill-buffer (&optional delete-buffer-p)
  "kill other window buffer.
If KILL-BUFFER-ONLY-P is nil, delete the other window.
Otherwise, the other window is not deleted."
  (interactive "P")
  (if (one-window-p)
      (message "one window")
    (let* ((win (next-window))
           (buf (window-buffer win)))
      (unless (eq (window-buffer) buf)
        (kill-buffer buf))
      (when delete-buffer-p
        (delete-window win)))))

;; 隣のウィンドウのバッファを削除し、ウィンドウを閉じ、
;; 現在のバッファも削除
(defun window-control-current-other-kill-buffer ()
  "Kill current buffer and the buffer of other window.
And, delete the other window."
  (interactive)
  (window-control-other-kill-buffer 1)
  (kill-buffer (current-buffer)))

(defun window-control-toggle-horizontal-vertical ()
  "Toggle split horizontally or vertically between
selected window and other window.
If selected window and other window is splitted vertically,
split them horizontally.
If splitted horizontally, vice versa."
  (interactive)
  (let ((selected-edges (window-edges (selected-window)))
        (other-edges (window-edges (next-window)))
        (other-buf (window-buffer (next-window))))
    (cond
     ((one-window-p) (message "One window, cannot toggle split."))
     ((and
       (= (nth 0 selected-edges) (nth 0 other-edges))
       (= (nth 2 selected-edges) (nth 2 other-edges)))
      (delete-window (next-window))
      (set-window-buffer (split-window-horizontally) other-buf))
     ((and
       (= (nth 1 selected-edges) (nth 1 other-edges))
       (= (nth 3 selected-edges) (nth 3 other-edges)))
      (delete-window (next-window))
      (set-window-buffer (split-window-vertically) other-buf)))))

;; 隣のウインドウとバッファを交換する
(defun window-control-swap ()
  "Swap buffers bettween selected window and other window."
  (interactive)
  (let ((other-buf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer (selected-window)))
    (set-window-buffer (selected-window) other-buf)))

;; 隣のウインドウを *scratch* バッファにする
(defun window-control-other-scratch ()
  "Open scratch buffer in next window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer "*scratch*")))

;; カレントディレクトリのシェルバッファを開く
(defun window-control-other-shell ()
  "Spilt the frame and switch the above window to
shell of default directory in current buffer."
  (interactive)
  (let (abuf)
    (save-window-excursion
      (setq abuf (shell)))
    (switch-to-buffer-other-window abuf)))

;; 隣のウインドウを *Messages* バッファにする
(defun window-control-other-message ()
  "Open Message buffer in next window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer "*Messages*")))

;; 隣のウィンドウを終了する
(defun window-control-other-quit ()
  "Quit other buffer and bury its buffer."
  (interactive)
  (quit-window nil (next-window)))

(defun window-control-resize ()
  "Resize window."
  (interactive)
  (if (one-window-p)
      (error "One window. cannot resize the window."))
  (let
      (
       (start-width (window-width))
       (start-height (window-height))
       (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
       (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
       key
       )
    (catch 'end-flag
      (while t
        (setq key
              (aref
               (read-key-sequence-vector
                (format
                 "Window size current:[%dx%d]; s)tart:[%dx%d] h)-x j)+y k)-y l)+x; q)uit."
                 (window-width) (window-height) start-width start-height))
               0))
        (cond
         ((member key '(?l right))
          (enlarge-window-horizontally dx))
         ((member key '(?h left))
          (shrink-window-horizontally dx))
         ((member key '(?j up))
          (enlarge-window dy))
         ((member key '(?k down))
          (shrink-window dy))
         ((member key '(?s))
          (progn
            (shrink-window-horizontally (- (window-width) start-width))
            (shrink-window (- (window-height) start-height))))
         ((member key '(?q))
          (progn
            (message "Window resize: quit")
            (throw 'end-flag t)))))))
  (run-hooks 'window-control-resize-hook))

(provide 'window-control)
;;; window-control.el ends here
