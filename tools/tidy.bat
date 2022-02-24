@echo off
 ::  File:       tidy.bat
 ::  Author:     Julian Orchard [hello@julianorchard.co.uk]
 ::  Tag Added:  2022-02-24
 ::  Desciption: Fixes a variety of file names, making windows bracketed 
 ::              file names not suck quite so much
setlocal enabledelayedexpansion

:: Ask the important questions
  set /p bs="Remove brackets and spaces? (y/n) "
  if %bs% == y ( 
  set /p br="Replace spaces with a dash? (y/n) " 
  )
  set /p re="Use command recursively?    (y/n) "
  if %re% == y (
    set re= /R 
  ) else (
    set re=
  )

:: Loop Files
for %re% %%a in (*) do (

  REM // file = f
    set f=%%a

  REM // Remove 
    if %bs% == y (
      set f=!f:^(=!
      set f=!f:^)=!
      if %br% == y (
        set f=!f: =-!
      ) else (
        set f=!f:^ =!
      )
    )

  REM // jpg file case fixes
    set f=!f:^JPG=jpg!
    set f=!f:^jpeg=jpg!

  REM // png fix cases
    set f=!f:^PNG=png!

  REM // HTML, not very used
    set f=!f:^htm=html!

  REM // pdf fix case
    set f=!f:^PDF=pdf!

  REM // replace original files
  REM // this causes the issue with 
  REM // the /R flag
    ren "%%a" !f!
)

endlocal
