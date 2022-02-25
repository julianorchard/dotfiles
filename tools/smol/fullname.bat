@echo off
::  File:       fullname.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: Get Full Name on Windows
::              https://community.spiceworks.com/topic/
::              949313-what-is-the-windows-variable-for-a-users-full-name-not-just-the-username
	for /f "tokens=2*" %%a in ('net user "%Username%" /domain ^| find /i "Full Name"') do set DisplayName=%%b
	echo %DisplayName%
