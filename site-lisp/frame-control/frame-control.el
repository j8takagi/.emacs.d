;;; frame-control.el -*- lexical-binding: t -*-

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

;; Control Emcas frame.

;; ■インストール方法
;; 1. LOAD-PATHで指定されているディレクトリにこのファイルをコピーする
;;
;; 2. Emacsの設定ファイル（~/.emacs.d/init.el など）に以下の行を追加する
;;
;; (require 'frame-control)

(defgroup frame-control nil
  "Frame control utilities."
  :prefix "frame-control-"
  :version "25.2"
  :group 'frames
  :group 'convenience
  )

(defun frame-control-workarea ()
  (cdr (assoc 'workarea (frame-monitor-attributes)))
    )

(defun frame-control-workarea-left ()
  (nth 0 (frame-control-workarea)))

(defun frame-control-workarea-top ()
  (nth 1 (frame-control-workarea)))

(defun frame-control-workarea-right ()
  (nth 2 (frame-control-workarea)))

(defun frame-control-workarea-bottom ()
  (nth 3 (frame-control-workarea)))

(defcustom frame-control-shift-right-size (/ (display-pixel-width) 72)
  "Shift right pixel unit size when new frame opens."
  :type 'natnum
  )

(defcustom frame-control-shift-down-size (/ (display-pixel-height) 72)
  "Shift down pixel unit size when new frame opens."
  :type 'natnum
  )


(defcustom frame-control-resize-hook nil
  "Hooks to run in `frame-control-resize' after finishing frame resize."
  :type 'hook
  )

(defcustom frame-control-move-hook nil
  "Hooks to run in `frame-control-move' after finishing frame rmove."
  :type 'hook
  )

(defun frame-control-resize ()
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
    (run-hooks 'frame-control-resize-hook))

(defun frame-control-move ()
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
      (run-hooks 'frame-control-resize-hook))


(defun frame-control-visible-all-frames ()
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

(defun frame-control-iconify-other-frames ()
  "Make all live frames except selected frame into an icon."
  (interactive)
  (let ((sfrm (selected-frame)))
    (mapcar
     (lambda (frm)
       (unless (eq frm sfrm)
         (iconify-frame frm)))
     (frame-list)
     )))

(defun frame-control-invisible-other-frames ()
  "Make all live frames except selected frame invisible."
  (interactive)
  (let ((sfrm (selected-frame)))
    (mapcar
     (lambda (frm)
       (unless (eq frm sfrm)
         (make-frame-invisible frm)))
     (frame-list)
     )))

(defun frame-control-xlfd-pixels (xlfd)
  "Get pixels of font size from font-spec string XLFD."
  (let ((elmts (split-string xlfd "-")))
    ;; xlfd 7th element: pixels of font size
    (string-to-number (nth 7 elmts))))

(defun frame-control-fontsize ()
  (let ((fontsize 0))
    (setq fontsize (frame-control-xlfd-pixels (cdr (assoc 'font default-frame-alist))))
    (when (= fontsize 0)
      (setq fontsize (frame-control-xlfd-pixels (frame-parameter nil 'font))))
    fontsize))

(defun frame-control-shift-rightdown (&optional frame)
  (let*
      (
       (aframe-alist (copy-alist default-frame-alist))
       (leftcell (assoc 'left aframe-alist))
       (topcell (assoc 'top aframe-alist))
       (awidth (cdr (assoc 'width aframe-alist)))
       (aheight (cdr (assoc 'height aframe-alist)))
       (afontwidth (frame-char-width))
       (afontheight (frame-char-height))
       (newleft 0) (newtop 0)
       )
    (when frame (ignore))
    (when (and leftcell awidth)
      (setq newleft (+ (cdr leftcell) frame-control-shift-right-size))
      (when (<= (+ newleft (* afontwidth awidth)) (frame-control-workarea-right))
        (setcdr leftcell newleft)))
    (when (and topcell aheight)
      (setq newtop (+ (cdr topcell) frame-control-shift-down-size))
      (when  (<= (+ newtop (* afontheight aheight)) (frame-control-workarea-bottom))
        (setcdr topcell newtop)))
    (setq default-frame-alist aframe-alist)
    `(,newleft ,newtop)))

(defun frame-control-shift-leftup (&optional frame)
  (let
      (
       (leftcell (assoc 'left default-frame-alist))
       (topcell (assoc 'top default-frame-alist))
       (newleft 0) (newtop 0)
       )
    (when frame (ignore))
    (when leftcell
      (setq newleft (- (cdr leftcell) frame-control-shift-right-size))
      (when (>= newleft (frame-control-workarea-left))
        (setcdr leftcell newleft))
    (when topcell
      (setq newtop (- (cdr topcell) frame-control-shift-down-size))
      (when (>= newtop (frame-control-workarea-top))
        (setcdr topcell newtop)))
    `(,newleft ,newtop))))

(add-hook 'before-make-frame-hook 'frame-control-shift-rightdown)

(add-hook 'delete-frame-functions 'frame-control-shift-leftup)

(provide 'frame-control)
;;; frame-control.el ends here
