@echo off
:: Usage
:: glazbenaPetica.bat

set DPI=1200
call :generate_pdf %1 %2
goto :eof

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:generate_pdf
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
gswin64c -dLabelWidth=20.3 -dLabelHeight=10 @ps2pdf-oid.opt -sOutputFile#oids/glazbenaPetica.pdf -fglazbenaPetica.ps 
goto :eof
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

