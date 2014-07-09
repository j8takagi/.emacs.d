(setq mmm-global-mode 'maybe)

(setq mmm-submode-decoration-level 3)

(set-face-background 'mmm-default-submode-face "#f0f0ff")

;;
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>\n"
    :back "\n?[ \t]+</style>")))

;;
(mmm-add-mode-ext-class nil "\\.html?\\'" 'embedded-css)

;;
(mmm-add-classes
 '((html-javascript
    :submode javascript-mode
    :front "<script[^>]*>\n"
    :back "[ \t]+</script>")))

(mmm-add-mode-ext-class nil "\\.html?\\'" 'html-javascript)
