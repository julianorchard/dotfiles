@echo off
::  File:       ll.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: It's LL

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
