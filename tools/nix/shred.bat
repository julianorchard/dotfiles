@echo off
::  File:       shred.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: It's a crude and slow way of shredding on Windows

:: Get the Filename
  set filename=%1
  set extra=%2
  shift
  shift
  if not [%extra%] == [] echo This command only accepts single arguments for the moment; one file at a time please!
  if [%filename%] == [] set /p "filename=Please enter filename: "
  if not exist %filename% (
    echo This file does not exist!
    exit /b
  )

:: Number of Chars in File
  for %%i in (%filename%) do @set chars=%%~zi
  echo This file contains %chars% characters.

:: Empty file and set charset
  <nul set /p=>%filename%
  set charstr=1234567890abcdefghijklmnopqrstuvwxyz
:: This took me a little while to figure out
  setlocal enabledelayedexpansion
  for /l %%a in (1, 1, %chars%) do (
    set /a randchar=!random! %%36
    call set randomcharacter=%%charstr:~!randchar!,1%%
    <nul set /p=!randomcharacter!>>!filename!
  )

