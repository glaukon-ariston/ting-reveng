@echo off
:: Usage
:: generate-oid.bat 15000 15001
set DPI=1200
for /l %%a in (%1,1,%2) do call :generate_pdf %%a
::for /l %%a in (%1,1,%2) do call :generate_png %%a
goto :eof

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:generate_pdf
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
gswin64c -dstartTingId=%1 -dLabelWidth=17.8 -dLabelHeight=10 @ps2pdf-oid.opt -sOutputFile#oids/tingid-%1.pdf -fgenerateOid.ps 
goto :eof
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:generate_png
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::gswin64c -dNOPAUSE -dBATCH -sDEVICE=pngmono -r3200 -o oids/tingid-%1.png oids/tingid-%1.pdf
gswin64c -dNOPAUSE -dBATCH -sDEVICE=pngalpha -r%DPI% -dGraphicsAlphaBits=1 -o oids/tingid-%1.png oids/tingid-%1.pdf
::magick -density %DPI% oids/tingid-%1.pdf oids/tingid-%1.png
goto :eof
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
