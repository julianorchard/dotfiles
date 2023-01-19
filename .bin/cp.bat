@echo off

:::   cp.bat  ---  in case I forget I'm using CMD

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: In case I forget I'm using CMD.

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

set i=%1
set o=%2
shift
shift
xcopy "%i%" "%o%"
