;;; window-control.el

;; Copyright (C) 2014  j8takagi

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

;; Emcasのフレームとウィンドウを制御する。

;; ■動作環境
;; Emacs 24.3で動作を確認

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
(defun wctl-window-resize ()
    "Resize window."
    (interactive)
    (if (one-window-p)
      (error "One window. cannot resize the window."))
    (let
        ((thiswindow (selected-window))
        (start-width (window-width))
        (start-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        action
        c)
      (catch 'end-flag
      (while t
        (setq action
           (read-key-sequence-vector
              (format
                "Window size current:[%dx%d]; s)tart:[%dx%d] h)-x j)+y k)-y l)+x; q)uit."
                (window-width) (window-height) start-width start-height)))
        (setq c (aref action 0))
        (cond
          ((= c ?l)
            (enlarge-window-horizontally dx))
          ((= c ?h)
            (shrink-window-horizontally dx))
          ((= c ?j)
            (enlarge-window dy))
          ((= c ?k)
            (shrink-window dy))
          ((= c ?s)
            (progn
              (shrink-window-horizontally (- (window-width) start-width))
              (shrink-window (- (window-height) start-height))))
          ((= c ?q)
            (progn
              (message "Window resize: quit")
              (throw 'end-flag t))))))))

(defun wctl-frame-resize ()
   "Resize frame"
   (interactive)
     (let
         ((thisframe (selected-frame))
         (start-width (frame-width))
         (start-height (frame-height))
         (default-width (cdr (assoc 'width default-frame-alist)))
         (default-height (cdr (assoc 'height default-frame-alist)))
         action
         c)
       (catch 'end-flag
       (while t
        (setq action
           (read-key-sequence-vector
              (format
                "Frame Size current:[%dx%d]; s)tart:[%dx%d]; d)efault:[%dx%d]; h)-x j)+y k)-y l)+x; q)uit."
                (frame-width) (frame-height)
                start-width start-height default-width default-height)))
        (setq c (aref action 0))
        (cond
          ((= c ?l)
            (set-frame-width thisframe (+ (frame-width) 1)))
          ((= c ?h)
            (set-frame-width thisframe (- (frame-width) 1)))
          ((= c ?j)
            (set-frame-height thisframe (+ (frame-height) 1)))
          ((= c ?k)
            (set-frame-height thisframe (- (frame-height) 1)))
          ((= c ?d)
            (progn
              (set-frame-width thisframe default-width))
              (set-frame-height thisframe default-height))
          ((= c ?s)
            (progn
              (set-frame-width thisframe start-width)
              (set-frame-height thisframe start-height)))
          ((= c ?q)
            (progn
              (message "Frame Resize: quit")
              (throw 'end-flag t))))))))

(defun wctl-frame-move ()
   "Move frame."
   (interactive)
     (let
         ((thisframe (selected-frame))
         (start-top (frame-parameter nil 'top))
         (start-left (frame-parameter nil 'left))
         (default-top (cdr (assoc 'top default-frame-alist)))
         (default-left (cdr (assoc 'left default-frame-alist)))
         action
         c)
       (catch 'end-flag
       (while t
        (setq action
           (read-key-sequence-vector
              (format
                "Frame move current:[%d, %d]; s)tart:[%d, %d]; d)efault:[%d, %d]; h)-x j)+y k)-y l)+x; q)uit."
                (frame-parameter nil 'left) (frame-parameter nil 'top)
                start-left start-top default-left default-top)))
        (setq c (aref action 0))
        (cond
          ((= c ?l)
           (modify-frame-parameters nil (list (cons 'left (+ (frame-parameter nil 'left) 10)))))
          ((= c ?h)
           (modify-frame-parameters nil (list (cons 'left (- (frame-parameter nil 'left) 10)))))
          ((= c ?j)
           (modify-frame-parameters nil (list (cons 'top (+ (frame-parameter nil 'top) 10)))))
          ((= c ?k)
           (modify-frame-parameters nil (list (cons 'top (- (frame-parameter nil 'top) 10)))))
          ((= c ?d)
            (progn
              (modify-frame-parameters nil (list (cons 'left default-left)))
              (modify-frame-parameters nil (list (cons 'top default-top)))))
          ((= c ?s)
            (progn
              (modify-frame-parameters nil (list (cons 'left start-left)))
              (modify-frame-parameters nil (list (cons 'top start-top)))))
          ((= c ?q)
            (progn
              (message "Frame Move: quit")
              (throw 'end-flag t))))))))

(add-hook 'before-make-frame-hook 'frame-shift-right)

(add-hook 'delete-frame-functions
          (lambda (frame)
            (frame-shift-left)))

(defvar frame-shift-size 20)

(defun frame-shift-right ()
  (let* ((leftcell (assoc 'left default-frame-alist))
        (topcell (assoc 'top default-frame-alist)))
    (setcdr leftcell (+ (cdr leftcell) frame-shift-size))
    (setcdr topcell (+ (cdr topcell) frame-shift-size))))

(defun frame-shift-left ()
  (let* ((leftcell (assoc 'left default-frame-alist))
        (left (cdr leftcell))
        (topcell (assoc 'top default-frame-alist))
        (top (cdr topcell)))
    (if (>= left frame-shift-size)
        (setcdr leftcell (- left frame-shift-size)))
    (if (>= top frame-shift-size)
        (setcdr topcell (- top frame-shift-size)))))

(define-key global-map "\C-\\" (make-sparse-keymap))     ; C-\ をプリフィックスキーに

(global-set-key "\C-\\w" 'wctl-window-resize)            ; ウィンドウの対話式サイズ調整
(global-set-key "\C-\\f" 'wctl-frame-resize)             ; フレームの対話式サイズ調整
(global-set-key "\C-\\m" 'wctl-frame-move)               ; フレームの対話式移動

(provide 'window-control)
;;; window-control.el ends here
