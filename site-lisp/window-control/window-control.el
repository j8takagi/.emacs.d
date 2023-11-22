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

(defgroup wctl nil
  "Windows and frame control utilities."
  :prefix "wctl-"
  :version "25.2"
  :group 'windows
  :group 'convenience
  )


(defcustom wctl-system-display-width 1440
  "Width pixel unit of system display."
  :type 'natnum
  )

(defcustom wctl-system-display-height 672
  "Height pixel unit of system display."
  :type 'natnum
  )

(defcustom wctl-frame-shift-right-size 20
  "Shift right pixel unit size when new frame opens."
  :type 'natnum
  )

(defcustom wctl-frame-shift-down-size 20
  "Shift down pixel unit size when new frame opens."
  :type 'natnum
  )

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


(defun wctl-visible-all-frames ()
  "Make all live frames visible."
  (interactive)
  (let ((sfrm (selected-frame)) (res nil))
    (setq
     res
     (mapcar
      (lambda (frm)
        (let ((vp (frame-visible-p frm)))
          (when (or (eq 'icon vp) (null vp))
            (make-frame-visible frm)
            (lower-frame frm))
          ))
      (frame-list)))
    (raise-frame sfrm)
    res
  ))

(defun wctl-iconify-other-frames ()
  "Make all live frames except selected frame into an icon."
  (interactive)
  (let ((sfrm (selected-frame)))
    (mapcar
     (lambda (frm)
       (unless (eq frm sfrm)
         (iconify-frame frm)))
     (frame-list)
     )))

(defun wctl-invisible-other-frames ()
  "Make all live frames except selected frame invisible."
  (interactive)
  (let ((sfrm (selected-frame)))
    (mapcar
     (lambda (frm)
       (unless (eq frm sfrm)
         (make-frame-invisible frm)))
     (frame-list)
     )))

(defun wctl-xlfd-pixels (xlfd)
  "Get pixels of font size from font-spec string XLFD."
  (let ((elmts (split-string xlfd "-")))
    ;; xlfd 7th element: pixels of font size
    (string-to-number (nth 7 elmts))))

(defun wctl-frame-fontsize ()
  (let ((fontsize 0))
    (setq fontsize (wctl-xlfd-pixels (cdr (assoc 'font default-frame-alist))))
    (when (= fontsize 0)
      (setq fontsize (wctl-xlfd-pixels (frame-parameter nil 'font))))
    fontsize))

(defun wctl-frame-shift-rightdown (&optional frame)
  (let
      (
       (leftcell (assoc 'left default-frame-alist))
       (topcell (assoc 'top default-frame-alist))
       (awidth (cdr (assoc 'width default-frame-alist)))
       (aheight (cdr (assoc 'height default-frame-alist)))
       (afontsize (wctl-frame-fontsize))
       (newleft 0) (newtop 0)
       )
    (when frame (ignore))
    (when (and leftcell awidth)
      (setq newleft (+ (cdr leftcell) wctl-frame-shift-right-size))
      (when (<= (+ newleft (* 0.5 afontsize awidth)) wctl-system-display-width)
        (setcdr leftcell newleft)))
    (when (and topcell aheight)
      (setq newtop (+ (cdr topcell) wctl-frame-shift-down-size))
      (when  (<= (+ newtop (* afontsize aheight)) wctl-system-display-height)
        (setcdr topcell newtop)))
    `(,newleft ,newtop)))

(defun wctl-frame-shift-leftup (&optional frame)
  (let
      (
       (leftcell (assoc 'left default-frame-alist))
       (topcell (assoc 'top default-frame-alist))
       (newleft 0) (newtop 0)
       )
    (when frame (ignore))
    (when leftcell
      (setq newleft (- (cdr leftcell) wctl-frame-shift-right-size))
      (when (>= newleft 0)
        (setcdr leftcell newleft))
    (when topcell
      (setq newtop (- (cdr topcell) wctl-frame-shift-down-size))
      (when (>= newtop 0)
        (setcdr topcell newtop)))
    `(,newleft ,newtop))))

(add-hook 'before-make-frame-hook 'wctl-frame-shift-rightdown)

(add-hook 'delete-frame-functions 'wctl-frame-shift-leftup)

(define-key global-map "\C-\\" (make-sparse-keymap))     ; C-\ をプリフィックスキーに

(global-set-key "\C-\\w" 'wctl-window-resize)            ; ウィンドウの対話式サイズ調整
(global-set-key "\C-\\f" 'wctl-frame-resize)             ; フレームの対話式サイズ調整
(global-set-key "\C-\\m" 'wctl-frame-move)               ; フレームの対話式移動

(provide 'window-control)
;;; window-control.el ends here
