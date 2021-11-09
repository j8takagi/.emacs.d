;;; dictionary_app.el ---

;; Copyright (C) 2020 by Kazuhito Takagi

;; Authors: Kazuhito Takagi
;; Keywords: dictionary.app mac

;;; Commentary: based on 雑多なメモ置き場 http://y0m0r.hateblo.jp/entry/20120212/1329052521
;;; Code:

;;;###autoload
(defun dictionary_app (word)
  "Mac OSの辞書.appを開き、WORDの単語を調べる"
  (interactive "sWord:")
  (save-window-excursion
    (shell-command (format "open dict://%s &" word) "*dict")))

;;;###autoload
(defun dictionary_app-at-point ()
  (interactive)
  (dictionary_app (word-at-point)))

(global-set-key (kbd "C-c w") 'dictionary_app)
(global-set-key (kbd "C-c W") 'dictionary_app-at-point)

(provide 'dictionary_app)
;;; dictionary_app.el ends here
