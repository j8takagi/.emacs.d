(require 'dired)
(require 'init-ediff)

;; 確認なしにディレクトリーを再帰的にコピーする
(setq dired-recursive-copies 'always)

;; sorter - diredでのソート
(require 'sorter)

;; dired-x - diredの拡張機能
(require 'dired-x)

;; wdired - ファイル名の編集を可能にする
(require 'wdired)

;;
(put 'dired-find-alternate-file 'disabled nil)

;; ediff
(defun dired-ediff-vc-latest-current ()
  "Run Ediff of file named on this line by comparing the latest version and current."
  (interactive)
  (let ((find-file-run-dired nil))
    (find-file (dired-get-file-for-visit))
    (ediff-vc-latest-current)))

;; image-dired
(require 'image-dired)

;; ediff-revison
(define-key dired-mode-map "\C-cw" 'wdired-change-to-wdired-mode)
(define-key dired-mode-map "E" 'dired-ediff-vc-latest-current)
(define-key dired-mode-map "\C-ce" 'ediff-revision)

(provide 'init-dired)
