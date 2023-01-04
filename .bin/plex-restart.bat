@echo off

:::   plex-restart.bat  ---  

:: Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

taskkill /im plex* /f

:: run plex-media-server.exe
