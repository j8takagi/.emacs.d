set EMACS=C:\opt\emacs\bin\emacs
set EMACSDIR=%HOME%\.emacs.d
copy %EMACSDIR%\init.el %EMACSDIR%\init.el~
copy init.el %EMACSDIR%\init.el
copy init.elc %EMACSDIR%\init.elc
rmdir /S /Q %EMACSDIR%\init.d
mkdir %EMACSDIR%\init.d
xcopy /D /S /EXCLUDE:setup-exclude.txt init.d\* "%EMACSDIR%\init.d"
rmdir /S /Q %EMACSDIR%\init.sys.d
mkdir %EMACSDIR%\init.sys.d
copy init.sys.d\init-w32.el %EMACSDIR%\init.sys.d\init-w32.el
rmdir /S /Q %EMACSDIR%\site-lisp
mkdir %EMACSDIR%\site-lisp
xcopy /D /S /EXCLUDE:setup-exclude.txt site-lisp\* "%EMACSDIR%\site-lisp"
rmdir /S /Q %EMACSDIR%\insert
mkdir %EMACSDIR%\insert
xcopy /D /S /Y insert\* "%EMACSDIR%\insert"
