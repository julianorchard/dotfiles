@echo off
::  File:       drives.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: Creates a shortcut for drives available... 
::              for example, to switch to J:\, you could just
::              type 'j' in the future

:: Create Shortcuts Dir
	set shortcuts=C:\CMD\shortcuts
	if not exist %shortcuts% ( mkdir %shortcuts% )

:: Set Path
::    setx PATH "%PATH%;C:\CMD\tools\shortcuts\"   & REM THIS NEEDS LOOKING AT

:: Create All The Shortcut Files
	for %%a in (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) do (
		if exist %%a:\ (
			echo @echo off>%shortcuts%\%%a.bat
			echo REM added automatically by script>>%shortcuts%\%%a.bat
			echo %%a:>>%shortcuts%\%%a.bat
			echo %%a:\ has been added as a shortcut
		) else (
			echo %%a:\ does not exist
		)
	)
