@echo off

:::   cmdrc.bat  ---  .bashrc for CMD hack

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: This is a method of having a custom prompt in CMD. It's opened
:: by AutoHotkey (see ahk/general.ahk for more information).

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:


for /f "tokens=* usebackq" %%f in (`git config user.email`) do (
  set email=%%f
)
set /p hours=<C:\cmd\hours\log\cmdrc.txt
set PROMPT=$STime:$S$T$H$H$H$S$S$S$S$S$SGit:$S%email%$_$S$S$P$S$G$S

cd %homepath%
CMD
