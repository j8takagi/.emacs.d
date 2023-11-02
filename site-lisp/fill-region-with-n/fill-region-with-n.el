;;; fill-region-with-n.el --- 

;; Copyright (C) 2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:

;;; Code:
;;; 一行あたりの文字数を指定してfill-regionする関数
;;; http://d.hatena.ne.jp/hack-3/20090604/1244100952
(defun fill-region-with-n (num)
  "1行あたりの文字数を指定してfill-regionする"
  (interactive "nfill-column value? ")
  (let ((fill-column num))
    (fill-region (region-beginning) (region-end)))
  )

(defun fill-region-with-40 ()
  "1行あたり40字でfill-regionする"
  (interactive)
  (let ((fill-column 40))
    (fill-region (region-beginning) (region-end))))

(provide 'fill-region-with-n)
;;; fill-region-with-n.el ends here
