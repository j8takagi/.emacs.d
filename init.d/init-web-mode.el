(require 'web-mode)

;; web-modeのface設定
(custom-set-faces
 '(web-mode-indent-style 1)
 '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face ((t (:foreground "#00c0e0" :weight bold))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))

;; HTMLタグのインデントを0に
(defun web-mode-init ()
  "Set configuration to Hooks for Web mode."
  (setq web-mode-markup-indent-offset 0))

(add-hook 'web-mode-hook 'web-mode-init)
(provide 'init-web-mode)
