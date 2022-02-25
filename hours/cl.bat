@echo off

:: Mainly Handles Input 

:: (mainly so we don't have to deal with weird 
:: `cscript` stuff; I struggle to handle empty and weird
:: arguments with cscript)

set inOut=%1
set inputTime=%2
set inputDate=%3
shift


  if [%inputTime%] == [] ( 
    set inputTime=empty
  )
  if [%inputDate%] == [] (
    set inputDate=empty
  )

:: No Args
  if [%inOut%] == [] (
    CScript C:\cmd\hours\bin\check.vbs normal //nologo 
    exit /b
  )
:: Verbose
  if %inOut% == v ( 
    CScript C:\cmd\hours\bin\check.vbs verbose //nologo 
    exit /b
  )
:: Quiet
	if %inOut% == q (
		CScript C:\cmd\hours\bin\check.vbs quiet //nologo
	)

:: Call clock.vbs ; in / out
  if %inOut% == in ( 
    CScript C:\cmd\hours\bin\clock.vbs in %inputTime% %inputDate% //nologo 
    exit /b
  )
  if %inOut% == out ( 
    CScript C:\cmd\hours\bin\clock.vbs out %inputTime% %inputDate% //nologo 
    exit /b
  )    

:: Call 'Fill'
  if %inOut% == fill (
    CScript C:\cmd\hours\bin\fill.vbs %inputTime% //nologo
    exit /b
    :: Although I'm passing 'input time' through, 
    :: this is really the 'date' that'll be put through
  )
