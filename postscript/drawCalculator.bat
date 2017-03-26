@echo off
call :generate_pdf
goto :eof

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:generate_pdf
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
gswin32c @ps2pdf-oid.opt -sOutputFile#oids/calculator.pdf -fdrawCalculator.ps
goto :eof
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

