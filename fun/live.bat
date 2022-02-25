@echo off
 ::  File:       live.bat
 ::  Author:     Julian Orchard [hello@julianorchard.co.uk]
 ::  Tag Added:  2022-02-24
 ::  Desciption: Effectively make the CMD a clock
:loop
	timeout /t 1 /nobreak >nul
	cls
	cmd
goto loop
