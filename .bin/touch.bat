@echo off

:::   touch.bat  ---  In case I forget I'm using CMD...

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

for %%A in (%*) do (
	if exist %%A (
		echo %%A already exists, could not create file.
	) else (
		copy NUL %%A >NUL
		echo %%A successfully created
	)
)
