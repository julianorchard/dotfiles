@echo off

:::   addpath.bat  ---  Add current path to PATH.

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:


  set add=%1
  shift
  if [%add%] == [] ( set add=%cd% )
  if exist %add% (
    set PATH="%PATH%;%add%"
  ) else (
    echo Error; the location %add% could not be found.
    echo Please try again.
  )
:: Refresh Env
C:\CMD\bin\refresh.bat
