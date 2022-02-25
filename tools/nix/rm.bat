@echo off
::  File:       rm.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: It's RM

for %%a in (%*) do (
  if exist %%a\NUL (rmdir "%%a" /S /Q) else (del "%%a")
)
