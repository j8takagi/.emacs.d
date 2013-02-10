;;; -*- Emacs-Lisp -*-
;;; 16進数での色一覧表示と色一覧からのコード挿入

;; *Installation
;;   [~/.emacs]
;;     (autoload 'list-hexadecimal-colors-display "color-selection"
;;       "Display hexadecimal color codes, and show what they look like." t)

(defvar color-selection-list-mode-hook nil
  "*List of functions to call when entering color selection list mode.")

(defvar color-selection-list-mode-map nil
  "Keymap for color selection list major mode.")

(if color-selection-list-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'color-selection-list-choose-color)
    (setq color-selection-list-mode-map map)))

;; 色の名前を挿入するときにつける prefix
;; [example]
;;     "#"  => #rrggbb
;;     "0x" => 0xrrggbb
(defvar color-selection-list-prefix-string "#"
  "*Prefix string of color code.")

(defvar color-selection-list-increment 32
  "*Increment of each color element.")

;; 色一覧バッファから色を選ぶ
;; もともといたバッファに色の名前の文字列を挿入する
(defun color-selection-list-choose-color ()
  "Choose the color that point is in."
  (interactive)
  (let ((color-name ""))
    (setq color-name (cdr (get-text-property (point) 'face)))
    (setq color-name (concat color-selection-list-prefix-string
                             (substring color-name 1 (length color-name))))
    (kill-buffer (current-buffer))
    (set-buffer color-selection-list-original-buffer)
    (insert color-name)))

;; リストに含まれる色の一覧を表示する
;; list: 色の名前の文字列のリスト
(defun color-selection-list-colors-display (list)
  (if (and (get-buffer "*ColorSelection*")
           (buffer-live-p (get-buffer "*ColorSelection*")))
      (kill-buffer (get-buffer "*ColorSelection*")))
  (set-buffer (get-buffer-create "*ColorSelection*"))
  (erase-buffer)
    (save-excursion
      (let (s)
        (while list
          (if (not (null s))
              (insert "\n"))
          (setq s (point))
          (insert (car list))
          (indent-to 20)
          (put-text-property s (point) 'face
                             (cons 'background-color (car list)))
          (setq s (point))
          (insert "  " (car list))
          (put-text-property s (point) 'face
                             (cons 'foreground-color (car list)))
          (setq list (cdr list)))))
    (setq buffer-read-only t)
    (color-selection-list-mode)
    (switch-to-buffer "*ColorSelection*"))

;; 16 進数の色一覧を表示する
(defun list-hexadecimal-colors-display ()
  (interactive)
  (let ((r 0)
        (g 0)
        (b 0)
        (delta 32)
        (color-list ()))
    (setq color-selection-list-original-buffer (current-buffer))
    (while (< r 256)
      (setq g 0)
      (while (< g 256)
        (setq b 0)
        (while (< b 256)
          (setq color-list (append color-list (list (format "#%.2x%.2x%.2x" r g b))))
          (setq b (+ b color-selection-list-increment)))
        (setq g (+ g color-selection-list-increment)))
      (setq r (+ r color-selection-list-increment)))
    (color-selection-list-colors-display color-list)))

(defun color-selection-list-mode ()
  (interactive)
  (kill-all-local-variables)
  (text-mode)
  (setq major-mode 'color-selection-list-mode)
  (setq mode-name "color selection list mode")
  (use-local-map color-selection-list-mode-map)
  (run-hooks 'color-selection-list-mode-hook))

