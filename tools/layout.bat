@echo off

:: Works with mklayout.bat and the
:: \CMD\tools\layouts folder

set instr=%1
shift
setlocal enabledelayedexpansion

if not exist "C:\CMD\tools\layouts\!instr!.txt" (
	echo.
	echo The template !instr! did not exist. Please try again.
) else (
	set /A counter=1
	::  the new way
	for /f "tokens=*" %%a in (C:\CMD\tools\layouts\!instr!.txt) do (
	  ::  0 start or no0
		if !counter! GEQ 10 (
			mkdir "!counter!. %%a"
		) else (
			mkdir "0!counter!. %%a"
		)
		set /A counter+=1
	)
)
