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
