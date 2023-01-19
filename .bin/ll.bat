@echo off

:::   ll.bat  ---  A nicer (I find) DIR

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: `dir` isn't it my muscle memory at all.

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

set location=%1
shift

if [%location%] == [] (
        echo.
        echo.Folders:
        dir /b /o /a:d .
        echo.
        echo.Files:
        dir /b /o /a:-d .
) else (
  echo.
  echo.Folders:
  dir /b /o /a:d %location%
  echo.
  echo.Files:
  dir /b /o /a:-d %location%
)
