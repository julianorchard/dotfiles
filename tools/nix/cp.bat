@echo off
::  File:       cp.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: It's CP

set in=%1
set out=%2
shift
shift
xcopy "%in%" "%out%" 
