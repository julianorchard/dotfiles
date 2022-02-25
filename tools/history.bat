@echo off
 ::  File:       history.bat
 ::  Author:     Julian Orchard [hello@julianorchard.co.uk]
 ::  Tag Added:  2022-02-24
 ::  Desciption: Search files before or after a certain date

set dat=%1
set sea=%2
shift
shift
if [%dat%] == [] (
  echo. 
  echo History search tool: "history arg1 arg2 arg3" - the order is important
  echo.
  echo   arg1     Enter a date in the format with +/- depending 
  echo            on whether you want before or after the date, 
  echo            then date formatted dd/MM/yyyy
  echo.
  echo   arg2     OPTIONAL: Search for file names and or 
  echo            extensions e.g. *.jpg or *.png
  echo.
  echo   arg3     OPTIONAL: Standard arguments for the command,
  echo            e.g. /S for recursive search; type forfiles /?
  echo            for full list of these commands
) else (
  if [%sea%] == [] (
    forfiles /S /D %dat% /C "cmd /c echo @path"
  ) else (
    forfiles /S /D %dat% /m %sea% /C "cmd /c echo @path"
  )
)
