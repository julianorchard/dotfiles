@echo off
set arg=%1
shift
if [%arg%] == [] ( 
  echo.
  echo Error; this command requires arguments. 
  echo.
) else (
:: Call clock.vbs
  cscript C:\CMD\tools\docxtopdf.vbs %* //nologo
)
