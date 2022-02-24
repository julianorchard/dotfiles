@echo off
 ::  File:       shortcut.bat
 ::  Author:     Julian Orchard [hello@julianorchard.co.uk]
 ::  Tag Added:  2022-02-24
 ::  Desciption: Create a shortcut to the current folder, and/or
 ::              set up some sensible defaults (bin, home, and dev)

:: Set Defaults
  set shortdir=C:\CMD\shortcuts
  if not exist %shortdir% mkdir %shortdir%
	:: Recycle Bin
	if not exist %shortdir%\bin.bat (
	  echo @echo off>%shortdir%\bin.bat
	  echo start shell:recyclebinfolder>>%shortdir%\bin.bat
	  echo Recycle Bin shortcut created!
	) else (
	  echo Recycle Bin shortcut is already established.
	)
	:: Home Dir
	if not exist %shortdir%\home.bat (
	  echo @echo off>%shortdir%\home.bat
	  echo C:^>nul>>%shortdir%\home.bat
	  echo cd %homepath%>>%shortdir%\home.bat
	  echo C:%homepath% shortcut created!
	) else (
	  echo C:%homepath% shortcut is already established.
	)
	:: C:\CMD Dir
	if not exist %shortdir%\dev.bat (
	  echo @echo off>%shortdir%\dev.bat
	  echo C:^>nul>>%shortdir%\dev.bat
	  echo cd \CMD\>>%shortdir%\dev.bat
	  echo C:\CMD shortcut created!
	) else (
	  echo C:\CMD shortcut is already established.
	)
	
  set sname=%1
  shift
  if [%sname%] == [] (
	echo. 
	echo If you are looking to set up shortcuts to this folder, 
	echo use this command like 'shortcut NAME'.
	exit /b
  )

:: Detect if .cmd or .bat are already
:: present; remove them if they are
  set snameext=%sname:~-3%
  if %snameext% == bat set sname=%sname:~0,-4%
  if %snameext% == cmd set sname=%sname:~0,-4%

:: Check if File Exists
  if exist "%shortdir%\%sname%.bat" (
    echo Error; shortcut already exists.
    exit /b
  )

:: Get the Drive
  set drive=%cd:~0,1%

:: Actually Write the Shortcut
:: Format of each command...
::    @echo off
::    %drive%:>nul
::    cd %cd%
  echo @echo off>%shortdir%\%sname%.bat
  echo %drive%:^>nul>>%shortdir%\%sname%.bat
  echo cd %cd%>>%shortdir%\%sname%.bat

:: Feedback to the User
  echo Shortcut should now be working!
  echo Try it by typing "%sname%" from another dir.
