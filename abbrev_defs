;;-*-coding: utf-8;-*-
(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement :count 0)
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("case" "" c-case :count 0)
    ("do" "" c-dowhile :count 0)
    ("elsee" "" c-else :count 0)
    ("elseif" "" c-elseif :count 0)
    ("for" "" c-for :count 0)
    ("if" "" c-if :count 0)
    ("switch" "" c-switch :count 0)
    ("while" "" c-while :count 0)
   ))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("dolist" "" emacs-lisp-dolist :count 0)
   ))

(define-abbrev-table 'eukleides-mode-abbrev-table
  '(
    ("psaxes" "p1 = point(0, 0)
p2 = point(5, 4)
\\psaxes[arrows=\"->\", linecolor=\"gray\", linewidth=\"0.01\"](p1,p2)" nil :count 0)
    ("sample1" "A B C triangle
draw(A, B, C); draw(incircle(A, B, C)); draw(bisector(B, A, C), dotted)
draw(bisector(A, B, C), dotted); draw(bisector(B, C, A), dotted)
" nil :count 0)
    ("sample2" "A B C triangle
a = projection(A, line(B, C))
b = projection(B, line(A, C))
c = projection(C, line(A, B))
draw(A, B, C); draw(a); draw(b); draw(c); draw(segment(A, a), dotted)
draw(segment(B, b), dotted); draw(segment(C, c), dotted)
draw(barycenter(A, B)); draw(barycenter(B, C))
draw(barycenter(C, A)); draw(circle(a, b, c))
" nil :count 0)
    ("sample3" "A B C isosceles
H = projection(C, line(A, B))
draw(A, B, C); draw(H); draw(segment(C, H), dashed)
mark(B, H, C, right); mark(segment(A, H))
mark(segment(B, H)); mark(segment(A, C), cross)
mark(segment(C, B), cross); mark(B, A, C, double); mark(C, B, A, double)
" nil :count 0)
    ("sample4" "A B C D square
A B E equilateral(4)
B F G equilateral(4, 30:)
draw(A, B, C, D); draw(A, B, E); draw(B, F, G); draw(line(E, F), dotted)
" nil :count 0)
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
" nil :count 0)
    ("sample6" "A B C D parallelogram
draw(segment(A, B), full, arrow); draw(segment(A, C), full, arrow)
draw(segment(A, D), full, arrow); draw(segment(B, C), dotted)
draw(segment(D, C), dotted)
" nil :count 0)
    ("sample7" "O = point(2, 2)
C = circle(O, 2)
A = point(6.5, 2)
c = circle(O, A)
I J intersection(C, c)
color(lightgray)
draw(line(A, I)); draw(line(A, J))
color(black)
draw(O, plus); draw(A); draw(C); draw(c, dotted)
" nil :count 0)
    ("sample8" "frame(-5, -4, 5, 4)
trace(t, 0, 3*360)
{ point(t/360, t:) }
" nil :count 0)
    ("sample9" "F = point(3, 1.5)
D = line(point(1, 0.5), -65:)
C = parabola(F, D)
draw(F); draw(D); draw(C)
" nil :count 0)
   ))

(define-abbrev-table 'global-abbrev-table
  '(
    ("git" "www.j8takagi.net:/home/git" nil :count 0)
   ))

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement :count 0)
    ("else" "else" c-electric-continued-statement :count 0)
    ("finally" "finally" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'objc-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'php-mode-abbrev-table
  '(
    ("ex" "extends" nil :count 0)
   ))

(define-abbrev-table 'pike-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

