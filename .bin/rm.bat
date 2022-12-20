@echo off

:::   rm.bat  ---  In case I forget I'm in CMD!

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

for %%a in (%*) do (
  if exist %%a\NUL (rmdir "%%a" /S /Q) else (del "%%a")
)
