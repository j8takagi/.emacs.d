emacs -batch -l recompile-elpa.el
set EMACSDIR=%HOME%\.emacs.d
copy %EMACSDIR%\init.el %EMACSDIR%\init.el~
copy init.el %EMACSDIR%\init.el
rmdir /S /Q %EMACSDIR%\init.d
xcopy /s /EXCLUDE:setup-exclude.txt "init.d" "%EMACSDIR%\init.d"
rmdir /S /Q %EMACSDIR%\init.sys.d
mkdir %EMACSDIR%\init.sys.d
copy init-w32.el %EMACSDIR%\init.sys.d\init-w32.el
rmdir /S /Q %EMACSDIR%\site-lisp
xcopy /s /EXCLUDE:setup-exclude.txt "site-lisp" "%EMACSDIR%\site-lisp"
mkdir %EMACSDIR%\elpa
xcopy /s "elpa" "%EMACSDIR%\elpa"
