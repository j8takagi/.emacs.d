;;; skeleton-pair-japanese.el --- 

;; Copyright (C) 2015,2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'skeleton)

;; 日本語の括弧ペアを定義
(dolist
    (pair
     '(
       (?（ _ "）")
       (?｛ _ "｝")
       (?「 _ "」")
       (?『 _ "』")
       (?〔 _ "〕")
       (?《 _ "》")
       (?［ _ "］")
       (?【 _ "】")
       (?〈 _ "〉")
       (?“ _ "”")
       ))
  (add-to-list 'skeleton-pair-alist pair))

;; 日本語の括弧について挿入を自動化
(dolist
    (key
     '(
       "(" "\"" "{" "[" "（" "｛" "「" "『" "〔" "《" "［" "【" "〈" "“"
       ))
  (global-set-key (kbd key) 'skeleton-pair-insert-maybe))

(provide 'skeleton-pair-japanese)
;;; skeleton-pair-japanese.el ends here
