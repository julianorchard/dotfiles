@echo off

:: Works with layout.bat and the
:: \CMD\tools\layouts folder

set instr=%1
shift
setlocal enabledelayedexpansion
set layoutsfolder=C:\CMD\layouts\
set loc=!layoutsfolder!!instr!

if [!instr!] == [] (
	echo.
	echo Uh-oh^^! This command needs instructions^^!
	echo.
) else (
	set /a counter=1
	copy NUL !loc!.txt >NUL
	:addMore
		set /a counter+=1
		echo/
		set /p input=What would you like to name the first file in the layout?
		echo/
		type "!input!" >>!loc!.txt
	if !counter! == 20 GoTo addMore
)
