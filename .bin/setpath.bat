@echo off

:::   setpath.bat  ---  Quick PATH setting.

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

reg add HKEY_CURRENT_USER\Environment /v PATH /d "%PATH%;C:\cmd;C:\cmd\ahk;C:\cmd\bin;C:\cmd\hours;C:\cmd\hours\bin;C:\cmd\shortcuts;C:\Users\%USERNAME%\Documents\Personal Admin\Vim\vim81;C:\Program Files\VideoLAN;C:\cmd\exe\;C:\MAMP\bin\ruby\bin\;C:\MAMP\bin\python\bin\"
call C:\cmd\bin\refresh.bat

:: * TODO: This would probably be better with PowerShell
::   SCHEDULED: <2022-12-19>
::   Julian <jorchard@pm.me>
