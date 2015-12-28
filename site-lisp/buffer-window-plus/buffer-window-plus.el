;;; other-window-bindings.el

;; Copyright (C) 2015, 2014  j8takagi

;; Author: j8takagi <j8takagi@nifty.com>
;; Keywords: Emacs elisp window buffer bindings

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

;; Emcasのウィンドウやバッファの追加機能

;; ■動作環境
;; Emacs 24.3で動作を確認

;; ■インストール方法
;; 1. LOAD-PATHで指定されているディレクトリにこのファイルをコピーする
;;
;; 2. Emacsの設定ファイル（~/.emacs.d/init.el など）に以下の行を追加する
;;
;; (require 'other-window-bindings)

;; ■使い方

;; ■hook

(require 'shell)

;; 隣のウィンドウのバッファを削除する。ウィンドウはそのまま
(defun kill-next-window-buffer ()
  "Kill next window buffer. The window is not deleted."
  (interactive)
  (delete-kill-next-window-buffer 1))

;; 隣のウィンドウのバッファを削除する。
;; 引数kill-buffer-only-pがnon-nilの場合、ウィンドウも閉じる
(defun delete-kill-next-window-buffer (kill-buffer-only-p)
  "kill next window buffer.
If KILL-BUFFER-ONLY-P is non nil, delete the next window.
Otherwise, the next window is not deleted."
  (interactive "P")
  (if (one-window-p)
      (message "one window")
    (let* ((win (next-window))
           (buf (window-buffer win)))
      (unless (eq  (window-buffer) buf)
        (kill-buffer buf))
      (unless kill-buffer-only-p
          (delete-window win)))))

;; 現在および隣のウィンドウのバッファを削除する。
;; 隣のウィンドウは閉じ、現在のウィンドウはそのまま
(defun delete-kill-current-next-window-buffer ()
  "Kill current buffer and the buffer of next window.
And, delete the next window."
  (interactive)
  (delete-kill-next-window-buffer nil)
  (kill-buffer (current-buffer)))

;; 隣のウィンドウの分割方法を、横と縦で切り替える
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; 隣のウィンドウを終了する
(defun quit-next-window ()
  "Quit next buffer and bury its buffer."
  (interactive)
  (quit-window nil (next-window)))

;; シェルにカレントディレクトリへのcdコマンドを送る
(defun shell-send-cd (directory)
  "Send cd command of default directory to shell."
  (interactive)
  (setq default-directory directory)
  (goto-char (point-max))
  (comint-kill-input)
  (insert (concat "cd '" (expand-file-name directory) "'"))
  (comint-send-input)
  (recenter 1)
  (goto-char (point-max)))

;; カレントディレクトリでシェルバッファを開く
(defun shell-current-directory ()
   "If shell buffer exists, change directory in shell
to default directory in current buffer.
Otherwise, open new shell buffer of the dafault directory."
   (interactive)
   (let* (
       (shell-buffer (get-buffer "*shell*"))
       (proc (get-buffer-process shell-buffer))
       (curbuf (current-buffer))
       (curdir default-directory)
       (cd-command (concat "cd " curdir)))
     (if shell-buffer
         (if (process-running-child-p proc)
             (message "Child process is running in the shell.")
           (progn
             (set-buffer shell-buffer)
             (shell-send-cd curdir)))
       (shell))))

;; 現在のバッファを、カレントディレクトリのシェルバッファに切り替える
(defun switch-to-shell-current-directory ()
  "Switch current buffer to shell buffer of
default directory."
  (interactive)
  (unless (string= (buffer-name) "*shell*")
    (progn
      (shell-current-directory)
      (switch-to-buffer (get-buffer "*shell*")))))

;; フレームを2分割にし、カレントディレクトリのシェルバッファを開く
(defun split-shell-current-directory ()
  "Spilt the frame and switch the above window to
shell of default directory in current buffer."
  (interactive)
  (unless (string= (buffer-name) "*shell*")
    (progn
      (delete-other-windows)
      (split-window-below)
      (switch-to-shell-current-directory))))

;; 新しいフレームに、カレントディレクトリのシェルバッファを開く
(defun new-frame-shell-current-directory ()
  "Make a new frame, and switch the new frame window
to shell of default directory in current buffer."
  (interactive)
  (unless (string= (buffer-name) "*shell*")
    (progn
      (make-frame-command)
      (switch-to-shell-current-directory))))

(defun toggle-split-next-window ()
  "Toggle split between selected window and next window.
If selected window and next window is splitted vertically, split them horizontally.
If splitted horizontally, vice versa."
  (interactive)
  (let ((selected-edges (window-edges (selected-window)))
        (next-edges (window-edges (next-window)))
        (next-buf (window-buffer (next-window))))
    (cond
     ((one-window-p) (message "One window, cannot toggle split."))
     ((and
       (= (nth 0 selected-edges) (nth 0 next-edges))
       (= (nth 2 selected-edges) (nth 2 next-edges)))
      (delete-window (next-window))
      (set-window-buffer (split-window-horizontally) next-buf))
     ((and
       (= (nth 1 selected-edges) (nth 1 next-edges))
       (= (nth 3 selected-edges) (nth 3 next-edges)))
      (delete-window (next-window))
      (set-window-buffer (split-window-vertically) next-buf)))))

;; 隣のウインドウとバッファを交換する
(defun swap-buffer-next-window ()
  "Swap buffers bettween selected window and next window."
  (interactive)
  (let ((next-buf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer (selected-window)))
    (set-window-buffer (selected-window) next-buf)))

;; 隣のウインドウを *scratch* バッファにする
(defun scratch-other-window ()
  "Open scratch buffer in next window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer "*scratch*")))

;; 新しいフレームを開き、*scratch*バッファにする
(defun new-frame-scratch ()
  "Open scratch buffer in new frame."
  (interactive)
  (switch-to-buffer-other-frame (get-buffer "*scratch*")))

;; 隣のウインドウを *Messages* バッファにする
(defun message-other-window ()
  "Open Message buffer in next window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer "*Messages*")))

;; 新しいフレームを開き、*Messages*バッファにする
(defun new-frame-messages ()
  "Open Message buffer in new frame."
  (interactive)
  (switch-to-buffer-other-frame (get-buffer "*Messages*")))

(provide 'buffer-window-plus)
;;; buffer-window-plus.el ends here
