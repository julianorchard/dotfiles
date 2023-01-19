@echo off

:::   shortcuts.bat  ---  Create shortcuts and set some sensible defaults

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: Use this script to create a shortcut to the current folder you're in.
:: I've found this especially useful for CMD navigation.

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

:: Defaults

set shortdir=C:\CMD\shortcuts
if not exist %shortdir% mkdir %shortdir%

:: Home Dir
if not exist %shortdir%\home.bat (
    echo @echo off>%shortdir%\home.bat
    echo C:^>nul>>%shortdir%\home.bat
    echo cd %homepath%>>%shortdir%\home.bat
    echo C:%homepath% shortcut created!
) else (
    echo C:%homepath% shortcut is already established.
)
:: C:\CMD Dir
if not exist %shortdir%\dev.bat (
    echo @echo off>%shortdir%\dev.bat
    echo C:^>nul>>%shortdir%\dev.bat
    echo cd \CMD\>>%shortdir%\dev.bat
    echo C:\CMD shortcut created!
) else (
    echo C:\CMD shortcut is already established.
)

set sname=%1
shift
if [%sname%] == [] (
    echo.
    echo If you are looking to set up shortcuts to this folder,
    echo use this command like 'shortcut NAME'.
    exit /b
)

set snameext=%sname:~-3%
if %snameext% == bat set sname=%sname:~0,-4%
if %snameext% == cmd set sname=%sname:~0,-4%

if exist "%shortdir%\%sname%.bat" (
    echo Error; a shortcut of this name already exists.
    exit /b
)

set drive=%cd:~0,1%

echo @echo off>%shortdir%\%sname%.bat
echo %drive%:^>nul>>%shortdir%\%sname%.bat
echo cd %cd%>>%shortdir%\%sname%.bat

:: Feedback to the User
echo Shortcut should now be working!
echo Try it by typing "%sname%" from another dir.
