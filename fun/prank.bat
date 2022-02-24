@echo off
::  File:       prank.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: Make many folders on the desktop (classic prank)
set instr=%1
shift
setlocal enabledelayedexpansion
if [!instr!] == [] (set "instr=lol_")
for /L %%a IN (1,1,500) do (
  mkdir %USERPROFILE%\Desktop\!instr!%%a
)
