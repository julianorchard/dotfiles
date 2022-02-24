@echo off

::  File:       refreshprompt.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: Copied from CMDRC.bat; used to refresh the prompt

:: Get Current Git Username
    for /f "tokens=* usebackq" %%f in (`git config user.email`) do (
      set email=%%f
    )
:: Basically PS1
    set PROMPT=$STime:$S$T$H$H$H$S$SGit:$S%email%$_$S$S$P$S$G$S

