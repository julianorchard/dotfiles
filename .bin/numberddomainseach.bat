@echo off

:::   numberddomainseach.bat  ---  An oddly specific tool.

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: Used to find out how many instances of a domain with numbers in
:: (e.g. test1.com, test2.com, ...) are live/pingable.

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

echo Numbered Domain Search
echo.
echo Very specific tool used to find out how many instances of a domain with
echo numbers after e.g. test1.com, test2.com, test3.com, etc. are live/pingable.
echo.

set /p start="Enter the start (e.g. thisdomain, not thisdomain.com): "
set /p end="Enter the end (e.g. .com, etc) if applicable: "
set /p no="Search to number... : "

echo Searching %start%X%end%, to number %no%...

for /l %%x in (1, 1, %no%) do (
    ping -n 1 %start%%%x%end% | find "TTL=" >nul
    if not errorlevel 1 ( echo %start%%%x%end% is online )
)
