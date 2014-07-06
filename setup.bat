set EMACSDIR=%HOME%\.emacs.d
rmdir /S /Q %EMACSDIR%
mkdir %EMACSDIR%
copy init.el %EMACSDIR%\init.el
copy init-w32.el %EMACSDIR%\init-w32.el
mkdir %EMACSDIR%\site-lisp
xcopy /s /EXCLUDE:setup-exclude.txt "site-lisp" "%EMACSDIR%\site-lisp\"
mkdir %EMACSDIR%\elpa
xcopy /s "elpa" "%EMACSDIR%\elps\"
