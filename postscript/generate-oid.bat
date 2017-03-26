@echo off
:: Usage
:: 		generate-oid.bat 15000 15100
set DPI=1200
for /l %%a in (%1,1,%2) do call :generate_pdf %%a
for /l %%a in (%1,1,%2) do call :generate_png %%a
goto :eof

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:generate_pdf
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
gswin32c -fgenerateOid.ps -dstartTingId=%1 -dLabelWidth=20 -dLabelHeight=20 -sOutputFile#oids/tingid-%1.pdf @ps2pdf-oid.opt
goto :eof
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:generate_png
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::gswin32c -dNOPAUSE -dBATCH -sDEVICE=pngmono -r3200 -o oids/tingid-%1.png oids/tingid-%1.pdf
gswin32c -dNOPAUSE -dBATCH -sDEVICE=pngalpha -r%DPI% -dGraphicsAlphaBits=1 -o oids/tingid-%1.png oids/tingid-%1.pdf
::magick -density %DPI% oids/tingid-%1.pdf oids/tingid-%1.png
goto :eof
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
