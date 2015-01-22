(require 'web-mode)

(define-skeleton web-template
  "template of web-mode HTML file."
  nil
  "<!DOCTYPE html>" n
  "<html>" n
  "<head>" n
  "<meta charset=\"utf-8\" />" n
  "<title>" _ "</title>" n
  "<link rel=\"stylesheet\" href=\"style.css\" />" n
  "</head>" n
  n
  "<body>" n
  "<h1></h1>" n
  "</body>" n
  "</html>" n
  )

(define-skeleton html-tag-title
  "HTML tag title"
  nil
  > "<title>" _ "</title>")

(define-skeleton html-tag-base-href
  "HTML tag base"
  nil
  > "<base href=\"" _ "\" />"
  )

(define-skeleton html-tag-base-target
  "HTML tag base"
  nil
  > "<base target=\"" _ "\" />")

(define-skeleton html-tag-link
  "HTML tag link"
  nil
  > "<link rel=\"" _ "\" href=\"\" />")

(define-skeleton html-tag-meta-name
  "HTML tag `meta' with attribute `name'."
  nil
  > "<meta name=\"" _ "\" content=\"\" />")

(define-skeleton html-tag-meta-http
  "HTML tag meta with attribute http-equiv."
  nil
  > "<meta http-equiv=\"" _ "\" content=\"\" />")

(define-skeleton html-tag-style-inline
  "HTML tag style"
  nil
  > "<style" _ " />")

(define-skeleton html-tag-style-block
  "HTML tag style"
  nil
  > "<style"> n
  _ n
  "</style>" n)

(define-skeleton html-tag-script-inline
  "HTML tag script"
  nil
  > "<script" _ " />")

(define-skeleton html-tag-script-block
  "HTML tag script"
  nil
  > "<script"> n
  _ n
  "</script>" n)

(define-skeleton html-tag-noscript-inline
  "HTML tag noscript"
  nil
  > "<noscript>" _ "</noscript>")

(define-skeleton html-tag-noscript-block
  "HTML tag noscript"
  nil
  > "<noscript"> n
  _ n
  "</noscript>" n)

(define-skeleton html-tag-section
  "HTML tag section"
  nil
  > "<section>" _ "</section>")

(define-skeleton html-tag-nav
  "HTML tag nav"
  nil
  > "<nav"> n
  _ n
  "</nav>" n)

(define-skeleton html-tag-article
  "HTML tag article"
  nil
  > "<article"> n
  _ n
  "</article>" n)

(define-skeleton html-tag-aside
  "HTML tag aside"
  nil
  > "<aside"> n
  _ n
  "</aside>" n)

(define-skeleton html-tag-h1
  "HTML tag h1"
  nil
  > "<h1>" _ "</h1>")

(define-skeleton html-tag-h2
  "HTML tag h2"
  nil
  > "<h2>" _ "</h2>")

(define-skeleton html-tag-h3
  "HTML tag h3"
  nil
  > "<h3>" _ "</h3>")

(define-skeleton html-tag-h4
  "HTML tag h4"
  nil
  > "<h4>" _ "</h4>")

(define-skeleton html-tag-h5
  "HTML tag h5"
  nil
  > "<h5>" _ "</h5>")

(define-skeleton html-tag-h6
  "HTML tag h6"
  nil
  > "<h6>" _ "</h6>")

(define-skeleton html-tag-header
  "HTML tag header"
  nil
  > "<header"> n
  _ n
  "</header>" n)

(define-skeleton html-tag-footer
  "HTML tag footer"
  nil
  > "<footer"> n
  _ n
  "</footer>" n)

(define-skeleton html-tag-address
  "HTML tag address"
  nil
  > "<address"> n
  _ n
  "</address>" n)

(define-skeleton html-tag-p-block
  "HTML tag p"
  nil
  > "<p"> n
  _ n
  "</p>" n)

(define-skeleton html-tag-p-inline
  "HTML tag p"
  nil
  > "<p>" _ "</p>")

(define-skeleton html-tag-hr
  "HTML tag hr"
  nil
  > "<hr>" n)

(define-skeleton html-tag-pre
  "HTML tag pre"
  nil
  > "<pre"> n
  _ n
  "</pre>" n)

(define-skeleton html-tag-blockquote
  "HTML tag blockquote"
  nil
  > "<blockquote"> n
  _ n
  "</blockquote>" n)

(define-skeleton html-tag-ol
  "HTML tag ol"
  nil
  > "<ol>" n
  "<li>" _ "</li>" n
  "</ol>" n)

(define-skeleton html-tag-ul
  "HTML tag ol"
  nil
  > "<ul>" n
  "<li>" _ "</li>" n
  "</ul>" n)

(define-skeleton html-tag-li
  "HTML tag li"
  nil
  > "<li>" _ "</li>")

(define-skeleton html-tag-dl
  "HTML tag dt"
  nil
  > "<dl>" n
  "<dt>" _ "</dt>" n
  "<dd></dd>" n
  "</dl>" n)

(define-skeleton html-tag-dt
  "HTML tag dt"
  nil
  > "<dt>" _ "</dt>")

(define-skeleton html-tag-dd
  "HTML tag dd"
  nil
  > "<dd>" _ "</dd>")

(define-skeleton html-tag-figure
  "HTML tag figure"
  nil
  > "<figure"> n
  _ n
  "</figure>" n)

(define-skeleton html-tag-figcaption
  "HTML tag figcaption"
  nil
  > "<figcaption>" _ "</figcaption>")

(define-skeleton html-tag-div
  "HTML tag div"
  nil
  > "<div"> n
  _ n
  "</div>" n)

(define-skeleton html-tag-a-href
  "HTML tag `a' with attribute `href'"
  nil
  > "<a href=\"" _ "\">" @ "</a>")

(define-skeleton html-tag-em
  "HTML tag em"
  nil
  > "<em>" _ "</em>")

(define-skeleton html-tag-strong
  "HTML tag strong"
  nil
  > "<strong>" _ "</strong>")

(define-skeleton html-tag-small
  "HTML tag small"
  nil
  > "<small>" _ "</small>")

(define-skeleton html-tag-s
  "HTML tag s"
  nil
  > "<s>" _ "</s>")

(define-skeleton html-tag-cite
  "HTML tag cite"
  nil
  > "<cite>" _ "</cite>")

(define-skeleton html-tag-q
  "HTML tag q"
  nil
  > "<q>" _ "</q>")

(define-skeleton html-tag-dfn
  "HTML tag dfn"
  nil
  > "<dfn>" _ "</dfn>")

(define-skeleton html-tag-abbr
  "HTML tag abbr"
  nil
  > "<abbr>" _ "</abbr>")

(define-skeleton html-tag-time
  "HTML tag time"
  nil
  > "<time>" _ "</time>")

(define-skeleton html-tag-code
  "HTML tag code"
  nil
  > "<code>" _ "</code>")

(define-skeleton html-tag-var
  "HTML tag var"
  nil
  > "<var>" _ "</var>")

(define-skeleton html-tag-samp
  "HTML tag samp"
  nil
  > "<samp>" _ "</samp>")

(define-skeleton html-tag-kbd
  "HTML tag kbd"
  nil
  > "<kbd>" _ "</kbd>")

(define-skeleton html-tag-sub
  "HTML tag sub"
  nil
  > "<sub>" _ "</sub>")

(define-skeleton html-tag-sup
  "HTML tag sup"
  nil
  > "<sup>" _ "</sup>")

(define-skeleton html-tag-i
  "HTML tag i"
  nil
  > "<i>" _ "</i>")

(define-skeleton html-tag-b
  "HTML tag b"
  nil
  > "<b>" _ "</b>")

(define-skeleton html-tag-u
  "HTML tag u"
  nil
  > "<u>" _ "</u>")

(define-skeleton html-tag-mark
  "HTML tag mark"
  nil
  > "<mark>" _ "</mark>")

(define-skeleton html-tag-ruby
  "HTML tag ruby"
  nil
  > "<ruby><rt>" _ "</rt><rp></rp>")

(define-skeleton html-tag-bdi
  "HTML tag bdi"
  nil
  > "<bdi>" _ "</bdi>")

(define-skeleton html-tag-bdo
  "HTML tag bdo"
  nil
  > "bdo dir=\"" _ "\"")

(define-skeleton html-tag-span
  "HTML tag span"
  nil
  > "<span>" _ "</span>")

(define-skeleton html-tag-br
  "HTML tag br"
  nil
  > "<br" _ " />")

(define-skeleton html-tag-wbr
  "HTML tag wbr"
  nil
  > "<wbr" _ " />")

(define-skeleton html-tag-ins
  "HTML tag ins"
  nil
  > "<ins>" _ "</ins>")

(define-skeleton html-tag-del
  "HTML tag del"
  nil
  > "<del>" _ "</del>")

(define-skeleton html-tag-img
  "HTML tag img"
  nil
  > "<img src=\"" _ "\" alt=\"\" />")

(define-skeleton html-tag-iframe-single
  "HTML tag iframe"
  nil
  > "<iframe" _ " />")

(define-skeleton html-tag-iframe-pair
  "HTML tag iframe"
  nil
  > "<iframe>" _ "</iframe>")

(define-skeleton html-tag-embed
  "HTML tag embed"
  nil
  > "<embed" _ " />")

(define-skeleton html-tag-object-single
  "HTML tag object"
  nil
  > "<object" _ " />")

(define-skeleton html-tag-object-pair
  "HTML tag object"
  nil
  > "<object>" _ "</object>")

(define-skeleton html-tag-object-block
  "HTML tag object"
  nil
  > "<object"> n
  _ n
  "</object>" n)

(define-skeleton html-tag-param
  "HTML tag param"
  nil
  > "<param" _ " />")

(define-skeleton html-tag-video-single
  "HTML tag video"
  nil
  > "<video" _ " />")

(define-skeleton html-tag-video-pair
  "HTML tag video"
  nil
  > "<video>" _ "</video>")

(define-skeleton html-tag-video-block
  "HTML tag video"
  nil
  > "<video"> n
  _ n
  "</video>" n)

(define-skeleton html-tag-audio-single
  "HTML tag audio"
  nil
  > "<audio" _ " />")

(define-skeleton html-tag-audio-pair
  "HTML tag audio"
  nil
  > "<audio>" _ "</audio>")

(define-skeleton html-tag-audio-block
  "HTML tag audio"
  nil
  > "<audio"> n
  _ n
  "</audio>" n)

(define-skeleton html-tag-source
  "HTML tag source"
  nil
  > "<source" _ " />")

(define-skeleton html-tag-track
  "HTML tag track"
  nil
  > "<track" _ " />")

(define-skeleton html-tag-canvas-single
  "HTML tag canvas"
  nil
  > "<canvas>" _ "</canvas>")

(define-skeleton html-tag-canvas-pair
  "HTML tag canvas"
  nil
  > "<canvas>" _ "</canvas>")

(define-skeleton html-tag-canvas-block
  "HTML tag canvas"
  nil
  > "<canvas"> n
  _ n
  "</canvas>" n)

(define-skeleton html-tag-map
  "HTML tag map"
  nil
  > "<map"> n
  _ n
  "</map>" n)

(define-skeleton html-tag-area
  "HTML tag area"
  nil
  > "<area" _ " />")

(define-skeleton html-tag-table
  "HTML tag table"
  nil
  > "<table"> n
  _ n
  "</table>" n)

(define-skeleton html-tag-caption
  "HTML tag caption"
  nil
  > "<caption>" _ "</caption>")

(define-skeleton html-tag-colgroup
  "HTML tag colgroup"
  nil
  > "<colgroup"> n
  _ n
  "</colgroup>" n)

(define-skeleton html-tag-col
  "HTML tag col"
  nil
  > "<col" _ " />")

(define-skeleton html-tag-tbody
  "HTML tag tbody"
  nil
  > "<tbody"> n
  _ n
  "</tbody>" n)

(define-skeleton html-tag-thead
  "HTML tag thead"
  nil
  > "<thead"> n
  _ n
  "</thead>" n)

(define-skeleton html-tag-tfoot
  "HTML tag tfoot"
  nil
  > "<tfoot"> n
  _ n
  "</tfoot>" n)

(define-skeleton html-tag-tr-inline
  "HTML tag tr"
  nil
  > "<tr>" _ "</tr>")

(define-skeleton html-tag-tr-block
  "HTML tag tr"
  nil
  > "<tr"> n
  _ n
  "</tr>" n)

(define-skeleton html-tag-td-inline
  "HTML tag td"
  nil
  > "<td>" _ "</td>")

(define-skeleton html-tag-td-block
  "HTML tag td"
  nil
  > "<td"> n
  _ n
  "</td>" n)

(define-skeleton html-tag-th-inline
  "HTML tag th"
  nil
  > "<th>" _ "</th>")

(define-skeleton html-tag-th-block
  "HTML tag th"
  nil
  > "<th"> n
  _ n
  "</th>" n)

;; Autoinserting
(define-auto-insert "\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" 'web-template)

(provide 'web-skeletons)
;;; web-skeletons.el ends here
