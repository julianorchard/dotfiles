@echo off
::  File:       pingport.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: similar to ping, see if a port is responding

set input=%1
shift

setlocal enabledelayedexpansion
set count=1

for /F "tokens=* USEBACKQ" %%F in (`curl -I -s %input%`) do (
  set m!count!="%%F"
  set /a count=!count!+1
)

:: Echo the response code
if not [!m1!] == [] (
  echo Response code !m1!.
) else (
  echo No response at all.
)

endlocal
