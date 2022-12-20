@echo off

:::   ps.bat  ---  Call PowerShell from a CMD Windows...

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: I need to open PowerShell with -ExecutionPolicy bypass
:: when I'm at work, because *REASONS*.

:: This also adds the -NoLogo flag when opening. It also
:: makes sure the PowerShell profile is loaded.

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:


set psdir="C:\%homepath%\Documents\WindowsPowerShell"

if not exist %psdir% mkdir %psdir%

if not exist %psdir%\Microsoft.PowerShell_profile.ps1 (
  xcopy C:\CMD\psrc.ps1 %psdir%\Microsoft.Powershell_profile.ps1 
) else (
  xcopy C:\CMD\psrc.ps1 %psdir%\Microsoft.PowerShell_profile.ps1 /Y > nul
)

echo.

powershell.exe -ExecutionPolicy bypass -NoLogo
