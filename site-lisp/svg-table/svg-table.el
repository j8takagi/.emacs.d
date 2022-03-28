;;; svg_table.el --- Proof of concept for showing a rich GUI with SVG overlay. -*- lexical-binding: t -*-

;; Copyright (C) 2021 taku0
;;
;; Author: taku0 (http://github.com/taku0)
;; Version: 2.1.0
;; Package-Requires: ((emacs "24.4") (json-mode "1.7.0"))
;; Keywords: abbrev, convenience, files
;; URL: https://github.com/taku0/json-par

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Proof of concept for showing a rich GUI with SVG overlay.
;;
;; This parses the buffer string,  layouts it as a table, and show it using SVG overlay.

;;; Code


;; Buffer format:
;;
;; file = header body
;; header = "TABLE" props "\n"
;; props = json-object-without-newline
;; body = row*
;; row = cell* "\n"? "\C-^" "\n"?   ; \C-^ is a unit separator
;; cell = /.*?/ "\n"? "\C-_" "\n"?  ; \C-_ is a record separator

(require 'svg)
(require 'dom)
(require 'xml)
(require 'cl-lib)
(require 'cl-macs)


;;; Data structures

;; Parsed AST

(cl-defstruct (svg-table-table
               (:constructor svg-table-table)
               (:copier svg-table-table-copy))
  rows)

(cl-defstruct (svg-table-row
               (:constructor svg-table-row)
               (:copier svg-table-row-copy))
  cells
  start
  end)

(cl-defstruct (svg-table-cell
               (:constructor svg-table-cell)
               (:copier svg-table-cell-copy))
  start
  end)


;; Layouted AST

(cl-defstruct (svg-table-table-layout
               (:constructor svg-table-table-layout)
               (:copier svg-table-table-layout-copy))
  row-layouts
  column-widths
  column-xs
  row-ys)

(cl-defstruct (svg-table-row-layout
               (:constructor svg-table-row-layout)
               (:copier svg-table-row-layout-copy))
  cell-layouts
  start
  end
  height)

(cl-defstruct (svg-table-cell-layout
               (:constructor svg-table-cell-layout)
               (:copier svg-table-cell-layout-copy))
  span-layouts
  start
  end
  content-width
  content-height)

(cl-defstruct (svg-table-span-layout
               (:constructor svg-table-span-layout)
               (:copier svg-table-span-layout-copy))
  start
  end
  x
  y
  width
  height
  font
  type)


;;; Parsing

(defun svg-table--parse ()
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (svg-table--parse-table)))

(defun svg-table--parse-table ()
  (let ((rows ()))
    (while (not (eobp))
      (push (svg-table--parse-row) rows))
    (svg-table-table :rows (reverse rows))))

(defun svg-table--parse-row ()
  (let ((start (point))
        end
        (cells ()))
    (while (and (not (svg-table--forward-row-end-marker))
                (not (eobp)))
      (push (svg-table--parse-cell) cells))
    (svg-table-row :cells (reverse cells)
                   :start start
                   :end (save-excursion
                          (svg-table--backward-row-end-marker)
                          (svg-table--backward-cell-end-marker)
                          (point)))))

(defun svg-table--parse-cell ()
  (let* ((region (svg-table-current-cell-region))
         (text (buffer-substring-no-properties (car region) (cdr region))))
    (goto-char (cdr region))
    (svg-table--forward-cell-end-marker)
    (svg-table-cell :start (car region)
                    :end (cdr region))))

;;; Layout

(defconst svg-table--fringe-left 30)
(defconst svg-table--header-gap 8)
(defconst svg-table--margin-left 5)
(defconst svg-table--margin-right 5)
(defconst svg-table--font-size 15)
(defconst svg-table--line-height 20)
(defconst svg-table--baseline-offset 15)

(defun svg-table--layout-table (table window)
  (let* ((row-layouts (mapcar
                       (lambda (row) (svg-table--layout-row row window))
                       (svg-table-table-rows table)))
         (column-widths (svg-table--compute-column-widths row-layouts))
         (column-xs
          (reverse (cl-reduce
                    (lambda (xs width)
                      (cons (+ (car xs) width) xs))
                    column-widths
                    :initial-value (list (+ svg-table--fringe-left
                                            svg-table--header-gap)))))
         (row-ys
          (reverse
           (cl-reduce
            (lambda (ys row-layout)
              (cons (+ (car ys) (svg-table-row-layout-height row-layout)) ys))
            (cdr row-layouts)
            :initial-value (list
                            (+ (svg-table-row-layout-height (car row-layouts))
                               svg-table--header-gap)
                            0)))))
    (setcar row-ys (- (car row-ys)
                      svg-table--header-gap))
    (svg-table-table-layout :row-layouts row-layouts
                            :column-widths column-widths
                            :column-xs column-xs
                            :row-ys row-ys)))

(defun svg-table--layout-row (row window)
  (let* ((cell-layouts (mapcar
                        (lambda (cell) (svg-table--layout-cell cell window))
                        (svg-table-row-cells row)))
         (height (apply #'max (mapcar
                               #'svg-table-cell-layout-content-height
                               cell-layouts))))
    (svg-table-row-layout :cell-layouts cell-layouts
                          :start (svg-table-row-start row)
                          :end (svg-table-row-end row)
                          :height height)))

(defun svg-table--compute-column-widths (row-layouts)
  (let ((done nil)
        (cell-layouts-lists
         (mapcar (lambda (row-layout)
                   (svg-table-row-layout-cell-layouts row-layout))
                 row-layouts))
        column
        (widths ()))
    (while (not done)
      (setq column (mapcar #'car cell-layouts-lists))
      (if (cl-some #'identity column)
          (progn
            (push (apply #'max
                         (mapcar
                          (lambda (cell-layout)
                            (if cell-layout
                                (svg-table-cell-layout-content-width
                                 cell-layout)
                              0))
                          column))
                  widths)
            (setq cell-layouts-lists (mapcar #'cdr cell-layouts-lists)))
        (setq done t)))
    (reverse widths)))

(defun svg-table--layout-cell (cell window)
  (let* ((point (point))
         (mark (and (use-region-p) (mark)))
         (start (svg-table-cell-start cell))
         (end (svg-table-cell-end cell))
         (has-region (and mark (<= start point end) (<= start mark end)))
         size
         (span-layouts ())
         font
         (span-start start)
         span-type
         (span-x 0)
         (span-y 0)
         (max-line-width 0)
         composition)
    (save-excursion
      (goto-char start)
      (setq span-type (cond
                       ((= (point) point) 'point)
                       ((and has-region (= (point) mark)) 'region)
                       (t 'default)))
      (setq font (font-at (point) window))
      (while (<= (point) end)
        (when (or (and (eq span-type 'point)
                       (/= span-start (point)))
                  (not (equal font (font-at (point) window)))
                  (and (= (point) point)
                       (not (eq span-type 'point)))
                  (eolp)
                  (= (point) end)
                  (and has-region (= (point) mark)))
          (setq size (window-text-pixel-size
                      window
                      span-start
                      (point)
                      (lsh 1 30)
                      (lsh 1 30)))
          (push (svg-table-span-layout
                 :start span-start
                 :end (point)
                 :x span-x
                 :y span-y
                 :width (car size)
                 :height (cdr size)
                 :font font
                 :type span-type)
                span-layouts)
          (setq span-start (point))
          (setq span-type (cond
                           ((= (point) point)
                            'point)
                           ((and has-region
                                 (<= (min point mark) (point))
                                 (< (point) (max point mark)))
                            'region)
                           (t
                            'default)))
          (setq span-x (+ span-x (car size)))
          (setq font (font-at (point) window)))
        (cond
         ((or (= (point) end) (eolp))
          (setq size (window-text-pixel-size
                      window
                      (line-beginning-position)
                      (point)
                      (lsh 1 30)
                      (lsh 1 30)))
          (setq max-line-width (max max-line-width (car size)))
          (when (= (point) point)
            (push (svg-table-span-layout
                   :start (point)
                   :end (1+ (point))
                   :x span-x
                   :y span-y
                   :width 1
                   :height (cdr size)
                   :font font
                   :type 'point)
                  span-layouts))
          (setq span-y (+ span-y (cdr size)))
          (when (eolp)
            (forward-char)
            (setq span-start (point))
            (setq span-type (cond
                             ((= (point) point)
                              'point)
                             ((and has-region
                                   (<= (min point mark) (point))
                                   (< (point) (max point mark)))
                              'region)
                             (t
                              'default)))
            (setq span-x 0)
            (setq font (font-at (point) window))))

         ((= (point) point)
          (setq composition (find-composition (point) (1+ (point))))
          (if composition
              (goto-char (nth 1 composition))
            (forward-char)))

         (t
          (forward-char)))))
    (svg-table-cell-layout
     :span-layouts (reverse span-layouts)
     :start start
     :end end
     :content-width (+ svg-table--margin-left
                       svg-table--margin-right
                       max-line-width)
     :content-height span-y)))


;;; Rendering

(defvar-local svg-table--overlay nil)

(defun svg-table--update-svg (window)
  "Update SVG in WINDOW.

Parse its contents, layout it, and put an SVG overlay on it."
  (unless svg-table--overlay
    (setq svg-table--overlay (make-overlay (point-min) (point-max))))
  (let* ((point (point))
         (mark (and (use-region-p) (mark)))
         (table (svg-table--parse))
         (table-layout (svg-table--layout-table table window))
         (svg (svg-table--render-table table-layout)))
    (overlay-put svg-table--overlay 'display (svg-image svg))
    (move-overlay svg-table--overlay (point-min) (point-max))))

(defun svg-table--render-table (table-layout)
  "Return SVG DOM for TABLE-LAYOUT."
  (let* ((row-layouts (svg-table-table-layout-row-layouts table-layout))
         (row-heights (mapcar #'svg-table-row-layout-height row-layouts))
         (column-widths (svg-table-table-layout-column-widths table-layout))
         (row-ys (svg-table-table-layout-row-ys table-layout))
         (column-xs (svg-table-table-layout-column-xs table-layout))
         (width (+ svg-table--fringe-left
                   svg-table--header-gap
                   (apply #'+ column-widths)
                   2))
         (height (+ svg-table--header-gap (apply #'+ row-heights) 2))
         (i 0)
         (y 0)
         (point-column-index (svg-table--column-index (point)))
         (mark-column-index (if (use-region-p)
                                (svg-table--column-index (mark))
                              point-column-index))
         (point-row-index (svg-table--row-index row-layouts (point)))
         (mark-row-index (if (use-region-p)
                             (svg-table--row-index row-layouts (mark))
                           point-row-index))
         (column-range (cons (min point-column-index mark-column-index)
                             (max point-column-index mark-column-index)))
         (row-range (cons (min point-row-index mark-row-index)
                          (max point-row-index mark-row-index)))
         (selection-x1 (nth (car column-range) column-xs))
         (selection-x2 (nth (1+ (cdr column-range)) column-xs))
         (selection-y1 (nth (car row-range) row-ys))
         (selection-y2 (if (zerop (cdr row-range))
                           (car row-heights)
                         (nth (1+ (cdr row-range)) row-ys)))
         rendered-header)
    (setq column-widths (vconcat column-widths))
    `(svg
      ((width . ,width)
       (height . ,height))
      ,(prog1 (svg-table--render-header
               (car row-layouts)
               y
               column-widths
               column-range
               row-range)
         (setq y (+ y
                    (svg-table-row-layout-height (car row-layouts))
                    svg-table--header-gap))
         (setq i (1+ i)))
      ,@(mapcar (lambda (row-layout)
                  (prog1 (svg-table--render-row
                          row-layout
                          i
                          y
                          column-widths
                          column-range
                          row-range)
                    (setq y (+ y (svg-table-row-layout-height row-layout)))
                    (setq i (1+ i))))
                (cdr row-layouts))
      (rect
       ((x . ,selection-x1)
        (y . ,selection-y1)
        (width . ,(- selection-x2 selection-x1))
        (height . ,(- selection-y2 selection-y1))
        (fill . "none")
        (stroke . "#07F")
        (stroke-width . 4))))))

(defun svg-table--render-header
    (row-layout y column-widths column-range row-range)
  (let ((x (+ svg-table--fringe-left svg-table--header-gap))
        (i 0)
        width
        (height (svg-table-row-layout-height row-layout)))
    `(g
      ()
      ,@(mapcar
         (lambda (cell-layout)
           (setq width (aref column-widths i))
           (prog1 (svg-table--render-cell
                   cell-layout
                   i
                   0
                   x
                   y
                   width
                   height
                   column-range
                   row-range
                   :fill (if (<= (car column-range)
                                 i
                                 (cdr column-range))
                             "#9CF"
                           "#FFF"))
             (setq x (+ x width))
             (setq i (1+ i))))
         (svg-table-row-layout-cell-layouts row-layout)))))

(defun svg-table--render-row (row-layout
                              index
                              y
                              column-widths
                              column-range
                              row-range)
  (let* ((start (svg-table-row-layout-start row-layout))
         (end (svg-table-row-layout-end row-layout))
         (x 0)
         (i 0)
         width
         (height (svg-table-row-layout-height row-layout)))
    `(g
      ()
      ,(prog1 (svg-table--render-row-heading
               index
               x
               y
               svg-table--fringe-left
               height
               :fill (if (<= (car row-range) index (cdr row-range))
                         "#9CF"
                       "#FFF"))
         (setq x (+ x svg-table--fringe-left svg-table--header-gap)))
      ,@(mapcar
         (lambda (cell-layout)
           (setq width (aref column-widths i))
           (prog1 (svg-table--render-cell
                   cell-layout
                   i
                   index
                   x
                   y
                   width
                   height
                   column-range
                   row-range)
             (setq x (+ x width))
             (setq i (1+ i))))
         (svg-table-row-layout-cell-layouts row-layout)))))

(defun svg-table--render-cell (cell-layout
                               column-index
                               row-index
                               x
                               y
                               width
                               height
                               column-range
                               row-range
                               &rest
                               style)
  (let* ((span-layouts (svg-table-cell-layout-span-layouts cell-layout))
         (point-span-layout
          (cl-find-if
           (lambda (span-layout)
             (eq (svg-table-span-layout-type span-layout) 'point))
           span-layouts))
         (region-span-layouts
          (cl-remove-if
           (lambda (span-layout)
             (not (eq (svg-table-span-layout-type span-layout) 'region)))
           span-layouts)))
    `(g
      ()
      (rect
       ((x . ,x)
        (y . ,y)
        (width . ,width)
        (height . ,height)
        (fill . ,(or (plist-get style :fill) "#FFF"))
        (stroke . "#CCC")
        (stroke-width . 1)))
      ,@(when point-span-layout
          (list
           `(rect
             ((x . ,(+ x
                       svg-table--margin-left
                       (svg-table-span-layout-x point-span-layout)))
              (y . ,(+ y
                       (svg-table-span-layout-y point-span-layout)))
              (width . ,(svg-table-span-layout-width point-span-layout))
              (height . ,(svg-table-span-layout-height point-span-layout))
              (fill . "#000")
              (stroke . "none")))))
      ,@(mapcar
         (lambda (span-layout)
           `(rect
             ((x . ,(+ x
                       svg-table--margin-left
                       (svg-table-span-layout-x span-layout)))
              (y . ,(+ y
                       (svg-table-span-layout-y span-layout)))
              (width . ,(if (and (eq (char-after
                                      (svg-table-span-layout-end span-layout))
                                     ?\n)
                                 (/= (point)
                                     (svg-table-span-layout-end span-layout))
                                 (/= (mark)
                                     (svg-table-span-layout-end span-layout)))
                            (- width
                               svg-table--margin-left
                               (svg-table-span-layout-x span-layout)
                               svg-table--margin-right)
                          (svg-table-span-layout-width span-layout)))
              (height . ,(svg-table-span-layout-height span-layout))
              (fill . "#EEE")
              (stroke . "none"))))
         region-span-layouts)
      (text
       ((stroke . "none"))
       ,@(mapcar
          (lambda (span-layout)
            (let* ((font (svg-table-span-layout-font span-layout))
                   (font-info (query-font font))
                   (font-family (font-get font :family))
                   (font-size (aref font-info 2))
                   (font-descent (aref font-info 5)))
              `(tspan
                ((x . ,(+ x
                          svg-table--margin-left
                          (svg-table-span-layout-x span-layout)))
                 (y . ,(+ y
                          (svg-table-span-layout-y span-layout)
                          (svg-table-span-layout-height span-layout)
                          (- font-descent)))
                 (fill . ,(if (eq (svg-table-span-layout-type span-layout)
                                  'point)
                              "#FFF"
                            "#000"))
                 (font-family . ,font-family)
                 (font-size . ,font-size))
                ,(xml-escape-string
                  (buffer-substring-no-properties
                   (svg-table-span-layout-start span-layout)
                   (svg-table-span-layout-end span-layout))))))
          span-layouts)))))

(defun svg-table--render-row-heading (index x y width height &rest style)
  (let* ((font (face-attribute 'default :font))
         (font-info (query-font font))
         (font-family (font-get font :family))
         (font-size (aref font-info 2))
         (font-descent (aref font-info 5)))
    `(g
      ()
      (rect
       ((x . ,x)
        (y . ,y)
        (width . ,width)
        (height . ,height)
        (fill . ,(or (plist-get style :fill) "#FFF"))
        (stroke . "#CCC")
        (stroke-width . "1")))
      (text
       ((stroke . "none")
        (x . ,(+ x
                 width
                 (- svg-table--margin-left)))
        (y . ,(+ y
                 height
                 (- font-descent)))
        (fill . "#000")
        (font-family . ,font-family)
        (font-size . ,font-size)
        (text-anchor . "end"))
       ,(xml-escape-string (number-to-string index))))))

;;; Inspect current cell

(defun svg-table-current-cell-region ()
  (save-excursion
    (cons (progn (svg-table-beginning-of-cell) (point))
          (progn (svg-table-end-of-cell) (point)))))

(defun svg-table-current-cell-text ()
  (let ((region (svg-table-current-cell-region)))
    (buffer-substring (car region) (cdr region))))

(defun svg-table--column-index (&optional point)
  "Return the column index of the cell containing POINT.

A column index of the first cell of a row is 0."
  (save-excursion
    (let ((colun-index 0)
          (done nil))
      (when point
        (goto-char point))
      (while (not done)
        (skip-chars-backward "^\C-^\C-_")
        (cond
         ((eq (char-before) ?\C-_)
          (setq colun-index (1+ colun-index))
          (backward-char))

         ((or (eq (char-before) ?\C-^) (bobp))
          (setq done t))))
      colun-index)))

(defun svg-table--row-index (row-layouts &optional point)
  "Return the row index of the cell containing POINT.

A row index of the first row (heading row) is 0."
  (unless point
    (setq point (point)))
  (cl-position-if
   (lambda (row-layout)
     (<= (svg-table-row-layout-start row-layout)
         point
         (svg-table-row-layout-end row-layout)))
   row-layouts))


;;; Movement

(defun svg-table--forward-cell-end-marker ()
  "Skip forward a cell end marker if any."
  (when (and (eq (char-after) ?\n)
             (eq (char-after (1+ (point))) ?\C-_))
    (forward-char))
  (when (eq (char-after) ?\C-_)
    (forward-char)
    (when (eq (char-after) ?\n)
      (forward-char))
    t))

(defun svg-table--forward-row-end-marker ()
  "Skip forward a row end marker if any."
  (when (and (eq (char-after) ?\n)
             (eq (char-after (1+ (point))) ?\C-^))
    (forward-char))
  (when (eq (char-after) ?\C-^)
    (forward-char)
    (when (eq (char-after) ?\n)
      (forward-char))
    t))

(defun svg-table--backward-cell-end-marker ()
  "Skip backward a cell end marker if any."
  (when (and (eq (char-before) ?\n)
             (eq (char-before (1- (point))) ?\C-_))
    (backward-char))
  (when (eq (char-before) ?\C-_)
    (backward-char)
    (when (eq (char-before) ?\n)
      (backward-char))
    t))

(defun svg-table--backward-row-end-marker ()
  "Skip backward a row end marker if any."
  (when (and (eq (char-before) ?\n)
             (eq (char-before (1- (point))) ?\C-^))
    (backward-char))
  (when (eq (char-before) ?\C-^)
    (backward-char)
    (when (eq (char-before) ?\n)
      (backward-char))
    t))

(defun svg-table-beginning-of-cell ()
  "Move point to the beginning of the current cell."
  (skip-chars-backward "^\C-^\C-_")
  (cond
   ((eq (char-after) ?\n)
    (forward-char))
   ((bobp)
    (forward-line))))

(defun svg-table-end-of-cell ()
  "Move point to the end of the current cell, excluding the cell end marker."
  (skip-chars-forward "^\C-^\C-_")
  (when (eq (char-before) ?\n)
    (backward-char)))

(defun svg-table-beginning-of-row ()
  "Move point to the beginning of the current row."  
  (skip-chars-backward "^\C-^")
  (cond
   ((eq (char-after) ?\n)
    (forward-char))
   ((bobp)
    (forward-line))))

(defun svg-table-end-of-row ()
  "Move point to the end of the current row, excluding the cell/row end marker."
  (skip-chars-forward "^\C-^")
  (when (eq (char-before) ?\n)
    (backward-char))
  (svg-table--backward-cell-end-marker))

(defun svg-table-forward-cell ()
  "Move point to the beginning of the next cell.

If the point is at the last cell of a row, move to the first cell of the next
row."
  (skip-chars-forward "^\C-^\C-_")
  (svg-table--forward-cell-end-marker)
  (svg-table--forward-row-end-marker))

(defun svg-table-backward-cell ()
  "Move point to the end of the previous cell.

If the point is at the first cell of a row, move to the last cell of the
previous row."
  (skip-chars-backward "^\C-^\C-_")
  (svg-table--backward-cell-end-marker)
  (svg-table--backward-row-end-marker)
  (when (bobp)
    (forward-line)))

(defun svg-table-forward-row ()
  "Move point to the beginning of the next row."
  (skip-chars-forward "^\C-^")
  (svg-table--forward-row-end-marker))

(defun svg-table-backward-row ()
  "Move point to the end of the previous row."
  (skip-chars-backward "^\C-^")
  (svg-table--backward-row-end-marker)
  (svg-table--backward-cell-end-marker)
  (when (bobp)
    (forward-line)))

(defun svg-table-goto-column (column-index)
  "Move point to the beginning of the COLUMN-INDEXth cell in the row."
  (let ((point (point))
        (done nil))
    (svg-table-beginning-of-row)
    (while (and (< 0 column-index)
                (not done))
      (skip-chars-forward "^\C-^\C-_")
      (svg-table--forward-cell-end-marker)
      (setq column-index (1- column-index))
      (when (svg-table--forward-row-end-marker)
        (goto-char point)
        (setq done t)))
    (zerop column-index)))

(defun svg-table-forward-char ()
  "Move point a character forward.

If the point is at the end of a cell, move to the next cell."
  (interactive)
  (forward-char)
  (svg-table--adjust-point 'forward))

(defun svg-table-backward-char ()
  "Move point a character backward.

If the point is at the beginning of a cell, move to the previous cell."
  (interactive)
  (backward-char)
  (svg-table--adjust-point 'backward))

(defun svg-table-next-line ()
  "Move point to the next visual line.

If the point is on the last line of a cell, move to the cell below."
  (interactive)
  (let ((point (point))
        char-column
        (end-of-cell (save-excursion
                       (svg-table-end-of-cell)
                       (point)))
        column-index)
    (recenter)
    (next-line)
    (when (< end-of-cell (point))
      (goto-char point)
      (setq column-index (svg-table--column-index))
      (setq char-column (current-column))
      (svg-table-forward-row)
      (if (eobp)
          (progn
            (goto-char point)
            (svg-table-end-of-cell))
        (if (svg-table-goto-column column-index)
            (move-to-column char-column)
          (goto-char point)
          (svg-table-end-of-cell))))))

(defun svg-table-previous-line ()
  "Move point to the previous visual line.

If the point is on the first line of a cell, move to the cell above."
  (interactive)
  (let ((point (point))
        char-column
        (beginning-of-cell (save-excursion
                             (svg-table-beginning-of-cell)
                             (point)))
        column-index)
    (recenter)
    (previous-line)
    (when (< (point) beginning-of-cell)
      (goto-char point)
      (setq column-index (svg-table--column-index))
      (setq char-column (current-column))
      (svg-table-beginning-of-row)
      (if (save-excursion
            (forward-line -1)
            (bobp))
          (progn
            (goto-char point)
            (svg-table-beginning-of-cell))
        (svg-table-backward-row)
        (if (svg-table-goto-column column-index)
            (progn
              (svg-table-end-of-cell)
              (move-to-column char-column))
          (goto-char point)
          (svg-table-beginning-of-cell))))))

(defun svg-table--adjust-point (direction)
  "If the point is outside of a cell, adjust it.

If DIRECTION is backward, prefer the previous cell.  Otherwise, prefer the next
cell."
  (cond
   ;; Inside the header line
   ((= (line-beginning-position) (point-min))
    (forward-line 1))

   ;; After last row
   ((save-excursion
      (skip-chars-forward "\s\t\n")
      (eobp))
    (svg-table-backward-row))
   
   ;; Inside a row marker
   ((or (and (eq (char-before) ?\C-^)
             (eq (char-after) ?\n))
        (and (eq (char-before) ?\n)
             (eq (char-after) ?\C-^)))
    (if (eq direction 'backward)
        (backward-char)
      (forward-char))
    (svg-table--adjust-point direction))

   ;; Inside a cell marker
   ((or (and (eq (char-before) ?\C-_)
             (eq (char-after) ?\n))
        (and (eq (char-before) ?\n)
             (eq (char-after) ?\C-_)))
    (if (eq direction 'backward)
        (backward-char)
      (forward-char))
    (svg-table--adjust-point direction))

   ;; Between a cell marker and a row marker
   ((and (save-excursion
           (skip-chars-backward "^\C-_\C-^")
           (eq (char-before) ?\C-_))
         (save-excursion
           (skip-chars-forward "^\C-_\C-^")
           (eq (char-after) ?\C-^)))
    (if (eq direction 'backward)
        (progn
          (skip-chars-backward "^\C-_")
          (svg-table--backward-row-end-marker))
      (skip-chars-forward "^\C-_\C-^")
      (svg-table--forward-row-end-marker)))))


;;; Initialization/deinitialization

(defun svg-table--initialize-view ()
  (let ((inhibit-read-only t))
    (widen)
    (goto-char (point-min))
    (put-text-property (point) (line-end-position) 'read-only t)
    (forward-line 1)
    (svg-table--show-body)
    (svg-table--hide-body)))

(defun svg-table--deinitialize-view ()
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max)
                            '(read-only t))
    (svg-table--show-body)))

(defun svg-table-deinitialize ()
  (svg-table--deinitialize-view))

(defun svg-table--pre-redisplay-function (window)
  (when (eq major-mode 'svg-table-mode)
    (svg-table--show-body)
    (unwind-protect
        ;; FIXME what if a buffer is displayed on multiple windows?
        (svg-table--update-svg window)
      (svg-table--hide-body))))

(defun svg-table--show-body ()
  (when svg-table--overlay
    (delete-overlay svg-table--overlay)))

(defun svg-table--hide-body ()
  (when svg-table--overlay
    (move-overlay svg-table--overlay (point-min) (point-max))))

(defun svg-table--post-command-hook ()
  (svg-table--adjust-point 'backward)
  (svg-table--hide-body))

;;; Keymap

(defvar svg-table-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map [remap forward-char] #'svg-table-forward-char)
    (define-key map [remap backward-char] #'svg-table-backward-char)
    ;; FIXME
    (define-key map [remap right-char] #'svg-table-forward-char)
    ;; FIXME
    (define-key map [remap left-char] #'svg-table-backward-char)
    (define-key map [remap next-line] #'svg-table-next-line)
    (define-key map [remap previous-line] #'svg-table-previous-line)
    map))


;;; The SVG Table mode

(define-derived-mode svg-table-mode text-mode "SVG-Table"
  "A major for a proof of consept of SVG GUI in Emacs.
\\{svg-table-mode-map}"
  (setq-local truncate-lines t)
  (setq-local global-disable-point-adjustment t)
  (setq-local line-move-ignore-invisible nil)
  (add-hook 'pre-redisplay-functions #'svg-table--pre-redisplay-function nil t)
  (add-hook 'change-major-mode-hook #'svg-table-deinitialize nil t)
  (add-hook 'pre-command-hook #'svg-table--show-body nil t)
  (add-hook 'post-command-hook #'svg-table--post-command-hook nil t)
  (svg-table--initialize-view))


(defun svg-table-demo ()
  (interactive)
  (pop-to-buffer "*SVG Table Demo*")
  (fundamental-mode)
  (delete-region (point-min) (point-max))
  (insert "TABLE {}\n-\n\C-_\nやること\n\C-_\n優先度\n\C-_\n説明\n\C-_\n\C-^\n☑\n\C-_\nデータフォーマットを決める\n\C-_\n高\n\C-_\nバッファのフォーマットを決める。\n各レコードは普通のテキストになるようにする。\nASCIIのRSやUSを使う?\n\C-_\n\C-^\n☑\n\C-_\nSVG生成をするフックを決める\n\C-_\n高\n\C-_\npre-redisplay-functions?\npost-command-hook?\n\C-_\n\C-^\n☑\n\C-_\nSVGで出力してみる\n\C-_\n高\n\C-_\nひとまず固定セルサイズで出力してみる。\n\C-_\n\C-^\n☑\n\C-_\nポイントを動かす関数を定義する\n\C-_\n高\n\C-_\nセルの外に出たらそっちにあるセルに移動する。\n\C-_\n\C-^\n☑\n\C-_\nテキストのサイズを測る\n\C-_\n高\n\C-_\n画面上でのテキストのサイズを測る。\n外部プログラムを使う?\nwindow-text-pixel-sizeが使える。\n\C-_\n\C-^\n☑\n\C-_\nパースとレンダリングに分ける\n\C-_\n高\n\C-_\nデータ構造を定義する。\nパースする。\nSVGに変換する。\n\C-_\n\C-^\n☑\n\C-_\nパースとレイアウトと\nレンダリングに分ける\n\C-_\n中\n\C-_\nレイアウトを別フェーズにする。\n\C-_\n\C-^\n☑\n\C-_\nポイントとリージョンを表示する\n\C-_\n中\n\C-_\nポイントやリージョンを表示する。\nセル内にリージョンが収まっている場合のみ。\nリージョンはセルの末尾まで伸びる。\n行末のポイントはスペースがないので狭くする。\n\C-_\n\C-^\n☐\n\C-_\n行や列を追加できるようにする\n\C-_\n中\n\C-_\n末尾に挿入用のセルを用意する。\nそこに入力したら行や列を追加する。\n表の途中に行や列を追加するコマンドを用意する。\n\C-_\n\C-^\n☐\n\C-_\n先頭や末尾の空行を表示する\n\C-_\n中\n\C-_\nセルの先頭や末尾に空行がある場合に表示されない。\n\C-_\n\C-^\n☐\n\C-_\n横スクロールする\n\C-_\n低\n\C-_\n大きなテーブルの場合、横スクロールできるようにする。\n\C-_\n\C-^\n☐\n\C-_\nセル内でテキストを折り返す\n\C-_\n低\n\C-_\n列の幅に合わせてテキストを折り返す設定ができるようにする。\n\C-_\n\C-^\n☐\n\C-_\nマウスで選択できるようにする\n\C-_\n低\n\C-_\nマウスのイベントを取得して、座標からテキスト位置やセル位置を計算する。\nポイントの移動やリージョンの設定やセルの選択ができるようにする。\n\C-_\n\C-^\n☐\n\C-_\n複数セルのキルやヤンク\n\C-_\n低\n\C-_\n複数セルを選択しているときはセル単位でキルする。\nキルしたものをヤンクできるようにする。\n\C-_\n\C-^\n☐\n\C-_\nリッチコンテンツの表示\n\C-_\n低\n\C-_\nテキスト以外をセル内に表示できるようにする。\n\C-_\n\C-^\n☐\n\C-_\n英字と日本語の位置を合わせる\n\C-_\n低\n\C-_\n1行に複数のフォントがある場合の上下位置を合わせる。\n現状は上揃え。\n\C-_\n\C-^\n☐\n\C-_\nオーバーレイを表示する\n\C-_\n低\n\C-_\nisearchとかSKKとかが設定するオーバーレイをSVG側に反映させる。\n\C-_\n\C-^\n☐\n\C-_\n高速化\n\C-_\n低\n\C-_\n画面に表示される部分のみ表示する。\n構文木やレイアウト結果を再利用する。\n\C-_\n\C-^\n")
  (svg-table-mode))

(provide 'svg-table)

;;; svg-table.el ends here
