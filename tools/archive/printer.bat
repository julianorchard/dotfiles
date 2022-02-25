@echo off
:: Works with printer.ahk

:: Archived in favour of a .ps1 version... 
:: and then a .vbs version
  for /f "tokens=*" %%a in (C:\CMD\ahk\res\printlist.txt) do (
    print %%a /D:\\PRINTER-LOCATION
    echo %%a
  )
