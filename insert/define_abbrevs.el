;;; define_abbrevs.el ---

;; Copyright (C) 2014 by j8takagi

;; Authors:Kazuhito Takagi
;; Keywords:

;;; Commentary:


;;; Code:
(require 'c-skeletons)
(require 'emacs-lisp-skeletons)
(require 'latex-skeletons)
(require 'web-skeletons)

;; Skeletons as Abbrev Expansions
(dolist
    (list
     '(
       (c-mode-abbrev-table
        "cc-mode"
        (
         ("/**" comment-javadoc-style)
         ("if" c-if)
         ("elseif" c-elseif)
         ("else" c-else)
         ("for" c-for)
         ("while" c-while)
         ))
       (emacs-lisp-mode-abbrev-table
        "lisp-mode"
        (
         ("dolist" emacs-lisp-dolist)
         ("defun" emacs-lisp-defun)
         ("defvar" emacs-lisp-defvar)
         ("template" emacs-lisp-template)
         ))
       (latex-mode-abbrev-table
        "tex-mode"
        (
         ("section" latex-section)
         ("subsection" latex-subsection)
         ("cite" latex-cite)
         ("table" latex-table)
         ("figure" latex-figure)
         ("itemize" latex-itemize)
         ("enumerate" latex-enumerate)
         ("quote" latex-quote)
         ("quotation" latex-quotation)
         ("eq" latex-equation)
         ("eqs" latex-eqnarray)
         ("eqn" latex-equation-nonumber)
         ("eqsn" latex-eqnarray-nonumber)
         ))
       (web-mode-abbrev-table
        "web-mode"
        (
         ("title" html-tag-title)
         ("base" html-tag-base-href)
         ("link" html-tag-link)
         ("meta" html-tag-meta-name)
         ("metahttp" html-tag-meta-http)
         ("style" html-tag-style-block)
         ("script" html-tag-script-block)
         ("noscript" html-tag-noscript-block)
         ("section" html-tag-section)
         ("nav" html-tag-nav)
         ("article" html-tag-article)
         ("aside" html-tag-aside)
         ("h1" html-tag-h1)
         ("h2" html-tag-h2)
         ("h3" html-tag-h3)
         ("h4" html-tag-h4)
         ("h5" html-tag-h5)
         ("h6" html-tag-h6)
         ("header" html-tag-header)
         ("footer" html-tag-footer)
         ("address" html-tag-address)
         ("p" html-tag-p-block)
         ("hr" html-tag-hr)
         ("pre" html-tag-pre)
         ("blockquote" html-tag-blockquote)
         ("ol" html-tag-ol)
         ("ul" html-tag-ul)
         ("li" html-tag-li)
         ("dl" html-tag-dl)
         ("dt" html-tag-dt)
         ("dd" html-tag-dd)
         ("figure" html-tag-figure)
         ("figcaption" html-tag-figcaption)
         ("div" html-tag-div)
         ("a" html-tag-a-href)
         ("em" html-tag-em)
         ("strong" html-tag-strong)
         ("small" html-tag-small)
         ("s" html-tag-s)
         ("cite" html-tag-cite)
         ("q" html-tag-q)
         ("dfn" html-tag-dfn)
         ("abbr" html-tag-abbr)
         ("time" html-tag-time)
         ("code" html-tag-code)
         ("var" html-tag-var)
         ("samp" html-tag-samp)
         ("kbd" html-tag-kbd)
         ("sub" html-tag-sub)
         ("sup" html-tag-sup)
         ("i" html-tag-i)
         ("b" html-tag-b)
         ("u" html-tag-u)
         ("mark" html-tag-mark)
         ("ruby" html-tag-ruby)
         ("rt" html-tag-rt)
         ("rp" html-tag-rp)
         ("bdi" html-tag-bdi)
         ("bdo" html-tag-bdo)
         ("span" html-tag-span)
         ("br" html-tag-br)
         ("wbr" html-tag-wbr)
         ("ins" html-tag-ins)
         ("del" html-tag-del)
         ("img" html-tag-img)
         ("iframe" html-tag-iframe-single)
         ("embed" html-tag-embed)
         ("object" html-tag-object-pair)
         ("param" html-tag-param)
         ("video" html-tag-video-single)
         ("audio" html-tag-audio-sigle)
         ("source" html-tag-source)
         ("track" html-tag-track)
         ("canvas" html-tag-canvas-single)
         ("map" html-tag-map)
         ("area" html-tag-area)
         ("table" html-tag-table)
         ("caption" html-tag-caption)
         ("colgroup" html-tag-colgroup)
         ("col" html-tag-col)
         ("tbody" html-tag-tbody)
         ("thead" html-tag-thead)
         ("tfoot" html-tag-tfoot)
         ("tr" html-tag-tr-inline)
         ("tr" html-tag-tr-block)
         ("td" html-tag-td-inline)
         ("td" html-tag-td-block)
         ("th" html-tag-th-inline)
         ("th" html-tag-th-block)
         ))
       ))
  (let ((table (car list)) (lib (nth 1 list)) (newabbrevs (nth 2 list)) extabb)
    (mapatoms
     (lambda (sym)
       (if (and (symbol-name sym) (fboundp sym))
           (add-to-list 'extabb `(,(symbol-name sym) ,(symbol-function sym)))))
     (eval table))
    (dolist (abb newabbrevs)
      (let*
          ((name (car abb)) (hook (nth 1 abb))
           (ext (assoc name extabb)) (updatep nil))
        (unless (and ext (equal hook (nth 1 ext)))
          (eval-after-load lib
            `(define-abbrev ,table ,name "" ',hook))
          (setq updatep t))
        (if updatep
            (eval-after-load lib (write-abbrev-file)))))))

(provide 'define_abbrevs)
;;; define_abbrevs.el ends here
