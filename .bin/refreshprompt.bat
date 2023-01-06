@echo off

:::   refreshprompt.bat  ---  Used whenever something in our prompt changes.

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: The main use case for this being whenever we change Git profiles. This isn't
:: something I do as often as I used to. 

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

for /f "tokens=* usebackq" %%f in (`git config user.email`) do (
set email=%%f
)
set /p hours=<C:\cmd\hours\log\cmdrc.txt
set PROMPT=$STime:$S$T$H$H$H$S$S$S$S$S$SGit:$S%email%$_$S$S$P$S$G$S
