@echo off

:::   amiconnected.bat  ---  Test internet connection.

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: Simply uses ping to check if we have an internet connection.

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

@setlocal enableextensions enabledelayedexpansion

echo.
echo Try running with 'notify' as an argument. It will
echo notify you via a popup when your connection is re-established...
echo.
set oldstate=Error
set notify=false
if [%1]==[] (
  set ipaddress=google.com
) else (
  if [%1]==[notify] (
    set ipaddress=google.com
    set notify=true
  ) else (
    set ipaddress=%1
  )
)

:loop
  set state=Down
  for /f "tokens=5,7" %%a in ('ping -n 1 !ipaddress!') do (
      if "x%%a"=="xReceived" if "x%%b"=="x1," set state=Up
  )
  if not !state!==!oldstate! (
    echo    Pinging: !ipaddress!    State: !state!
    set oldstate=!state!
  )
  pushd \CMD\bin
  if [%2]==[notify] set notify=true
  if [!notify!]==[true] (
    if [!state!]==[Up] (
    :: Uses msgBox.vbs to notify
      WScript msgBox.vbs Connected
      goto :end
    )
  )
  ping -n 2 127.0.0.1 >nul: 2>nul:
goto :loop
:end
endlocal
