;;; window-control.el

;; Copyright (C) 2014, 2017  j8takagi

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


(defvar wctl-window-resize-hook)

(defvar wctl-frame-resize-hook)

(defvar wctl-frame-move-hook)

(defun wctl-window-resize ()
  "Resize window."
  (interactive)
  (if (one-window-p)
      (error "One window. cannot resize the window."))
  (let
      (
       (thiswindow (selected-window))
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
  (run-hooks 'wctl-window-resize-hook))

(defun wctl-frame-resize ()
  "Resize frame"
  (interactive)
  (let
      (
       (thisframe (selected-frame))
       (start-width (frame-width))
       (start-height (frame-height))
       (default-width (cdr (assoc 'width default-frame-alist)))
       (default-height (cdr (assoc 'height default-frame-alist)))
       (default "")
       key
       )
    (when (and default-width default-height)
      (setq default (format " d)efault:[%dx%d];" default-width default-height)))
    (catch 'end-flag
      (while t
        (setq key
              (aref
               (read-key-sequence-vector
                (format
                 "Frame Size current:[%dx%d]; s)tart:[%dx%d]; %s h)-x j)+y k)-y l)+x; q)uit."
                 (frame-width) (frame-height)
                 start-width start-height default))
               0))
        (cond
         ((member key '(?l right))
          (set-frame-width thisframe (+ (frame-width) 1)))
         ((member key '(?h left))
          (set-frame-width thisframe (- (frame-width) 1)))
         ((member key '(?j down))
          (set-frame-height thisframe (+ (frame-height) 1)))
         ((member key '(?k up))
          (set-frame-height thisframe (- (frame-height) 1)))
         ((member key '(?d))
          (cond
           ((not default-width)
            (message "Width is not set in default-frame-alist."))
           ((not default-height)
            (message "Height is not set in default-frame-alist."))
           (t
            (progn
              (set-frame-width thisframe default-width)
              (set-frame-height thisframe default-height)))))
         ((member key '(?s))
          (progn
            (set-frame-width thisframe start-width)
            (set-frame-height thisframe start-height)))
         ((member key '(?q))
          (progn
            (message "Frame Resize: quit")
            (throw 'end-flag t)))))))
    (run-hooks 'wctl-frame-resize-hook))

(defun wctl-frame-move ()
  "Move frame."
  (interactive)
  (let
      (
       (thisframe (selected-frame))
       (start-top (frame-parameter nil 'top))
       (start-left (frame-parameter nil 'left))
       (default-top (cdr (assoc 'top default-frame-alist)))
       (default-left (cdr (assoc 'left default-frame-alist)))
       key
       )
    (catch 'end-flag
      (while t
        (setq key
              (aref
               (read-key-sequence-vector
                (format
                 "Frame move current:[%d, %d]; s)tart:[%d, %d]; d)efault:[%d, %d]; h)-x j)+y k)-y l)+x; q)uit."
                 (frame-parameter nil 'left) (frame-parameter nil 'top)
                 start-left start-top default-left default-top))
               0))
        (cond
         ((member key '(?l right))
          (modify-frame-parameters nil (list (cons 'left (+ (frame-parameter nil 'left) 10)))))
         ((member key '(?h left))
          (modify-frame-parameters nil (list (cons 'left (- (frame-parameter nil 'left) 10)))))
         ((member key '(?j down))
          (modify-frame-parameters nil (list (cons 'top (+ (frame-parameter nil 'top) 10)))))
         ((member key '(?k up))
          (modify-frame-parameters nil (list (cons 'top (- (frame-parameter nil 'top) 10)))))
         ((member key '(?d))
          (progn
            (modify-frame-parameters nil (list (cons 'left default-left)))
            (modify-frame-parameters nil (list (cons 'top default-top)))))
         ((member key '(?s))
          (progn
            (modify-frame-parameters nil (list (cons 'left start-left)))
            (modify-frame-parameters nil (list (cons 'top start-top)))))
         ((member key '(?q))
          (progn
            (message "Frame Move: quit")
            (throw 'end-flag t)))))))
      (run-hooks 'wctl-frame-resize-hook))

(add-hook 'before-make-frame-hook 'frame-shift-right)

(add-hook 'delete-frame-functions
          (lambda (frame)
            (frame-shift-left)))

(defvar frame-shift-size 20)

(defun frame-shift-right ()
  (let (leftcell topcell)
  (when (setq leftcell (assoc 'left default-frame-alist))
    (setcdr leftcell (+ (cdr leftcell) frame-shift-size)))
  (when (setq topcell (assoc 'top default-frame-alist))
    (setcdr topcell (+ (cdr topcell) frame-shift-size)))))

(defun frame-shift-left ()
  (let (leftcell topcell left top)
    (when (and
           (setq leftcell (assoc 'left default-frame-alist))
           (>= (setq left (cdr leftcell)) frame-shift-size))
      (setcdr leftcell (- left frame-shift-size)))
    (when (and
           (setq topcell (assoc 'top default-frame-alist))
           (>= (setq top (cdr topcell)) frame-shift-size))
      (setcdr topcell (- top frame-shift-size)))))

(define-key global-map "\C-\\" (make-sparse-keymap))     ; C-\ をプリフィックスキーに

(global-set-key "\C-\\w" 'wctl-window-resize)            ; ウィンドウの対話式サイズ調整
(global-set-key "\C-\\f" 'wctl-frame-resize)             ; フレームの対話式サイズ調整
(global-set-key "\C-\\m" 'wctl-frame-move)               ; フレームの対話式移動

(provide 'window-control)
;;; window-control.el ends here
