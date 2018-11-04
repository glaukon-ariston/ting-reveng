@echo off
:: Usage
:: generateContactSheet.bat 15000 100

:: Do no harm.
if [%1]==[] (
	echo generateContactSheet.bat startTingId periodTingId
	goto :EOF
)

if [%2]==[] (
	echo generateContactSheet.bat startTingId periodTingId
	goto :EOF
)

set DPI=1200
call :generate_pdf %1 %2
goto :eof

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:generate_pdf
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
gswin64c -dstartTingId=%1 -dperiodTingId=%2 -dLabelWidth=20.3 -dLabelHeight=10 @ps2pdf-oid.opt -sOutputFile#oids/tingid-%1-%2.pdf -fgenerateContactSheet.ps 
goto :eof
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

