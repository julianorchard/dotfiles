@echo off
::  File:       touch.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: It's Touch

for %%A in (%*) do (
	if exist %%A (
		echo %%A already exists, could not create file
	) else (
		copy NUL %%A >NUL
		echo %%A successfully created
	)
)
