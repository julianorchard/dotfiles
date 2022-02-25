@echo off
::  File:       removegitfilehistory.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: Remove file history for a single file in a repo

:: Goto Git Path
    for /f "tokens=* USEBACKQ" %%f in (`git rev-parse --show-toplevel`) do (
      set gitpath=%%f
    )
    cd %gitpath%

:: Find File Remove Full Path
    set /p fileremove="What is the name of the file you would like to remove the history of? "
    for /f "tokens=* USEBACKQ" %%f in (`dir *%fileremove%* /s /b /o`) do (
      set filetoremove=%%f
    )
    echo %filetoremove%
    set /p checkit="Are you SURE you want to remove the file? This is a permanent change (please enter 'yes' in full to proceed): "
    if %checkit%==yes ( git filter-branch --force --index-filter "git rm --cached --ignore-unmatch %filetoremove%" --prune-empty --tag-name-filter cat -- --all )
