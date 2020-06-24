;;; japanese-plus.el --- 

;; Copyright (C) 2016 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun japanese-plus-hankaku-region (regexp from to)
  "Convert REGEXP `zenkaku' chars in the region to `hankaku' chars."
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((hankaku (get-char-code-property (preceding-char) 'ascii)))
          (if hankaku
              (japanese-replace-region (match-beginning 0) (match-end 0)
                                       hankaku)))))))

;;;###autoload
(defun japanese-plus-normal-alphabet-region (from to)
  "Convert `zenkaku' letters in the region to normal letters."
  (interactive "r")
  (japanese-plus-hankaku-region "[Ａ-Ｚａ-ｚ]" from to))

;;;###autoload
(defun japanese-plus-normal-number-region (from to)
  "Convert `zenkaku' digits in the region to normal digits."
  (interactive "r")
  (japanese-plus-hankaku-region "[０-９]" from to))

;;;###autoload
(defun japanese-plus-normal-alnum-region (from to)
  "Convert `zenkaku' letters and digits in the region to normal letters and digits."
  (interactive "r")
  (japanese-plus-hankaku-region "[Ａ-Ｚａ-ｚ０-９]" from to))

;;;###autoload
(defun japanese-plus-zenkaku-kana-region (from to)
  "Convert Japanese `hankaku kana' chars in the region to `zenkaku kana' chars."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\ck" nil t)
        (let* ((hankaku (preceding-char))
               (composition (get-char-code-property hankaku 'kana-composition))
               next slot)
          (if (and composition (setq slot (assq (following-char) composition)))
              (japanese-replace-region (match-beginning 0) (1+ (point))
                                       (cdr slot))
            (let ((zenkaku (japanese-zenkaku hankaku)))
              (if zenkaku
                  (japanese-replace-region (match-beginning 0) (match-end 0)
                                           zenkaku)))))))))

;;;###autoload
(defun japanese-plus-normal-alnum-kana-region (from to)
  "Convert `zenkaku' letters and digits to normal letters and digits and Japanese `hankaku kana' chars in the region to `zenkaku kana' chars in the region."
  (interactive "r")
  (japanese-plus-normal-alnum-region from to)
  (japanese-plus-zenkaku-kana-region from to))

(defalias 'japanese-plus-normal-all 'japanese-plus-normal-alnum-kana-region)

(provide 'japanese-plus)
;;; japanese-plus.el ends here
