@echo off
::  File:       renr.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: REN (rename) recursively

set arg1=%1
set arg2=%2
shift
shift

if [%arg1%] == [] echo Recursive Rename! ^
  This script requires arguments ^(format is "renr match replace"^)

for /r %%x in (%arg1%) do ren "%%x" %arg2%
