;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-server.el
;;
(defconst elscreen-server-version "0.0.1 (July 25, 2006)")
;;
;; Author:   Hideyuki Shirai <shirai@meadowy.org>
;;           Naoto Morishima <naoto@morishima.net>
;; Created:  July 25, 2006

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'elscreen-server)
(require 'elscreen)

(defsubst elscreen-server-visit-files-new-screen (buffer-list)
  (elscreen-goto (car (mapcar
                       (lambda (buffer)
                         (elscreen-find-screen-by-buffer buffer 'create))
                       buffer-list)))
  (elscreen-notify-screen-modification 'force-immediately))

(unless (fboundp 'server-visit-files)
  (defun elscreen-server-find-buffer-visiting (filename)
    (if (file-directory-p filename)
        (car (dired-buffers-for-dir filename))
      (find-buffer-visiting filename))))

(cond
 ((fboundp 'server-visit-files)
  ;; For server.el distributed with GNU Emacs
  (defadvice server-visit-files (after elscreen-server-visit-files activate)
    (elscreen-server-visit-files-new-screen (cdr ad-return-value))))
 ((fboundp 'gnuserv-edit-files)
  ;; For (current) gnuserv typically used with XEmacs
  (defadvice gnuserv-edit-files (around elscreen-gnuserv-edit-files activate)
    (let ((filename-list (mapcar 'cdr list))
          (gnuserv-frame t))
      (save-window-excursion ad-do-it)
      (elscreen-server-visit-files-new-screen
       (mapcar 'elscreen-server-find-buffer-visiting filename-list)))))
 ((fboundp 'server-find-file)
  ;; For (ancient) gnuserv typically used with Meadow
  (defadvice server-edit-files (around elscreen-server-edit-files activate)
    (let ((filename-list (mapcar 'cdr list))
          (gnuserv-frame (selected-frame)))
      (save-window-excursion ad-do-it)
      (elscreen-server-visit-files-new-screen
       (mapcar 'elscreen-server-find-buffer-visiting filename-list))))
  (defadvice server-edit-files-quickly (around elscreen-server-edit-files-quickly activate)
    (let ((filename-list (mapcar 'cdr list))
          (gnuserv-frame (selected-frame)))
      (save-window-excursion ad-do-it)
      (elscreen-server-visit-files-new-screen
       (mapcar 'elscreen-server-find-buffer-visiting filename-list))))))
