;; http://sky-y.hatenablog.jp/entry/20120805/1344169124
(require 'ucs-normalize)

;; バッファ全体の濁点分離を直す
(defun ucs-normalize-NFC-buffer ()
  "Normalize current buffer by the Unicode NFC."
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max)))

(provide 'ucs-normalize-plus)
