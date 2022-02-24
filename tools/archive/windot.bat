@echo off

set instr=%1
shift

setlocal enabledelayedexpansion
:: Array of folders, messy solution
	set  folders=C:\CMD C:\CMD\ahk C:\CMD\ahk\res C:\CMD\ahk\temp C:\CMD\bin C:\CMD\fun C:\CMD\fun\archive C:\CMD\hours C:\CMD\hours\bin C:\CMD\hours\log C:\CMD\routine C:\CMD\tools C:\CMD\tools\archive C:\CMD\tools\layouts C:\CMD\tools\nix C:\CMD\tools\smol C:\CMD\tools\wip C:\CMD\vba C:\CMD\vba\events C:\CMD\vba\mail C:\CMD\vba\print

if [!instr!] == [] (
	echo This command needs instructions : -
	echo -install                    moves commands to your path
	echo -uninstall            removes the commands to your path
) else if !instr! == -install (
	(for %%a in (!folders!) do (
		set foldpath=%%a
		set relativepath=!foldpath:~7!
		if not exist !foldpath! mkdir !foldpath!
  REM // haven't actually tested this method 
    reg add HKEY_CURRECT_USER\Environment /v PATH /d "%PATH%;!foldpath!"
    call bin\refresh.bat >nul 2>&1
  REM // if this doesn't update the path correctly, 
  REM // use C:\CMD\tools\fixpath.bat
		echo !foldpath! - added to path 
		if [!relativepath!] == [] (
			xcopy * C:\CMD\ /q /f /y >nul 2>&1
			echo !foldpath! - copying root content to this location
		) else ( 
			xcopy !relativepath!\* !foldpath! /q /f /y >nul 2>&1
			echo !foldpath! - copying !relativepath! 
		)
	))
	:: Task Scheduling 
		schtasks /Create /tn "Screen Reminder" /tr "C:\CMD\routine\take-a-break.vbs" /sc hourly /st 00:00:00 /sd 01/01/2010 /ru %USERNAME%	
) else if !instr! == -uninstall (

	(for %%a in (!folders!) do ( 
		del   %%a\*	/q
		rmdir %%a
	))
	
	del C:\CMD\*
	rmdir C:CMD
) else (
	echo.
	echo ERROR, this command accepts the following instructions:
	echo -install                    moves commands to your path
	echo -uninstall            removes the commands to your path
)
