;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'apropos-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("else" "" c-else 0)
    ("elseif" "" c-elseif 0)
    ("if" "" c-if 0)
   ))

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'conf-colon-mode-abbrev-table '())

(define-abbrev-table 'conf-javaprop-mode-abbrev-table '())

(define-abbrev-table 'conf-ppd-mode-abbrev-table '())

(define-abbrev-table 'conf-space-mode-abbrev-table '())

(define-abbrev-table 'conf-unix-mode-abbrev-table '())

(define-abbrev-table 'conf-windows-mode-abbrev-table '())

(define-abbrev-table 'conf-xdefaults-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'edebug-eval-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("dolist" "" emacs-lisp-dolist 0)
   ))

(define-abbrev-table 'eukleides-mode-abbrev-table
  '(
    ("psaxes" "p1 = point(0, 0)
p2 = point(5, 4)
\\psaxes[arrows=\"->\", linecolor=\"gray\", linewidth=\"0.01\"](p1,p2)" nil 0)
    ("sample1" "A B C triangle
draw(A, B, C); draw(incircle(A, B, C)); draw(bisector(B, A, C), dotted)
draw(bisector(A, B, C), dotted); draw(bisector(B, C, A), dotted)
" nil 0)
    ("sample2" "A B C triangle
a = projection(A, line(B, C))
b = projection(B, line(A, C))
c = projection(C, line(A, B))
draw(A, B, C); draw(a); draw(b); draw(c); draw(segment(A, a), dotted)
draw(segment(B, b), dotted); draw(segment(C, c), dotted)
draw(barycenter(A, B)); draw(barycenter(B, C))
draw(barycenter(C, A)); draw(circle(a, b, c))
" nil 0)
    ("sample3" "A B C isosceles
H = projection(C, line(A, B))
draw(A, B, C); draw(H); draw(segment(C, H), dashed)
mark(B, H, C, right); mark(segment(A, H))
mark(segment(B, H)); mark(segment(A, C), cross)
mark(segment(C, B), cross); mark(B, A, C, double); mark(C, B, A, double)
" nil 0)
    ("sample4" "A B C D square
A B E equilateral(4)
B F G equilateral(4, 30:)
draw(A, B, C, D); draw(A, B, E); draw(B, F, G); draw(line(E, F), dotted)
" nil 0)
    ("sample5" "A B C D parallelogram(6, 4, 103:)
draw(A, B, C, D)
mark(B, A, D); mark(D, C, B); mark(C, B, A, double)
mark(A, D, C, double)
draw(A, C, dotted); draw(B, D, dotted)
thickness(3)
E = barycenter(A, B, C, D)
draw(A); draw(B); draw(C); draw(D); draw(E)
label(A, 250:); label(B, 310:); label(C, 45:); label(D, 120:)
label(E, 0.5, 270:)
" nil 0)
    ("sample6" "A B C D parallelogram
draw(segment(A, B), full, arrow); draw(segment(A, C), full, arrow)
draw(segment(A, D), full, arrow); draw(segment(B, C), dotted)
draw(segment(D, C), dotted)
" nil 0)
    ("sample7" "O = point(2, 2)
C = circle(O, 2)
A = point(6.5, 2)
c = circle(O, A)
I J intersection(C, c)
color(lightgray)
draw(line(A, I)); draw(line(A, J))
color(black)
draw(O, plus); draw(A); draw(C); draw(c, dotted)
" nil 0)
    ("sample8" "frame(-5, -4, 5, 4)
trace(t, 0, 3*360)
{ point(t/360, t:) }
" nil 0)
    ("sample9" "F = point(3, 1.5)
D = line(point(1, 0.5), -65:)
C = parabola(F, D)
draw(F); draw(D); draw(C)
" nil 0)
   ))

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("git" "www.j8takagi.net:/home/git")))

(define-abbrev-table 'graphviz-dot-mode-abbrev-table '())

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'html-helper-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'image-dired-display-image-mode-abbrev-table '())

(define-abbrev-table 'image-dired-thumbnail-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
   ))

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'makefile-automake-mode-abbrev-table '())

(define-abbrev-table 'makefile-bsdmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-gmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-imake-mode-abbrev-table '())

(define-abbrev-table 'makefile-makepp-mode-abbrev-table '())

(define-abbrev-table 'makefile-mode-abbrev-table '())

(define-abbrev-table 'mediawiki-draft-mode-abbrev-table '())

(define-abbrev-table 'mediawiki-mode-abbrev-table '())

(define-abbrev-table 'nxml-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
   ))

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'php-mode-abbrev-table
  '(
    ("ex" "extends" nil 0)
   ))

(define-abbrev-table 'pike-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
   ))

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'ruby-mode-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'svn-log-edit-mode-abbrev-table '())

(define-abbrev-table 'svn-log-view-mode-abbrev-table '())

(define-abbrev-table 'svn-status-diff-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'vc-annotate-mode-abbrev-table '())

(define-abbrev-table 'vc-dired-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'web-mode-abbrev-table '())

