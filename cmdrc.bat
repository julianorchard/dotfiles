@echo off
::  File:       cmdrc.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: Similar to .bashrc
::              File used to change the look of the windows terminal
::              Launched with Alt + Enter, from an AutoHotkey script
:: https://superuser.com/questions/144347/
:: is-there-windows-equivalent-to-the-bashrc-file-in-linux/916478#916478

:: Get Current Git Username
    for /f "tokens=* usebackq" %%f in (`git config user.email`) do (
      set email=%%f
    )

:: Get Hours Worked from C:\cmd\hours
   set /p hours=<C:\cmd\hours\log\cmdrc.txt

:: Basically PS1
   set PROMPT=$STime:$S$T$H$H$H$S$S$S$S$S$SGit:$S%email%$S$S$S$S$S$SHours:$S%hours%/37.5$_$S$S$P$S$G$S

:: Start in Home Dir...
    cd %homepath%

:: Start CMD
    CMD
