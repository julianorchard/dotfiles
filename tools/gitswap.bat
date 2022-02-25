@echo off

::  File:       gitswap.bat
::  Author:     Julian Orchard [hello@julianorchard.co.uk]
::  Tag Added:  2022-02-24
::  Desciption: Swap Git Accounts (two max)

set sshpath=C:\Users\%USERNAME%\.ssh

if not exist %sshpath%\temp (
  echo .ssh isn't set up with two accounts! Key-gen them.
  exit /b
)

:: Copy Current SSH Keys To Temp Folder
  move /Y %sshpath%\id_ed25519 %sshpath%\temp\id_ed25519_t >nul
  move /Y %sshpath%\id_ed25519.pub %sshpath%\temp\id_ed25519.pub_t >nul 

:: Copy Previously Temp SSH Keys The The Current Folder
  move /Y %sshpath%\temp\id_ed25519 %sshpath%\id_ed25519 >nul
  move /Y %sshpath%\temp\id_ed25519.pub %sshpath%\id_ed25519.pub >nul

:: Rename Temp Folder SSH Keys 
  ren %sshpath%\temp\id_ed25519_t id_ed25519
  ren %sshpath%\temp\id_ed25519.pub_t id_ed25519.pub

:: Git Test
  ssh -T git@github.com 

:: Set New Git Global Details
  set /P name="Enter your GitHub Username: "
  set /P email="Enter your GitHub Email: "
  git config --global user.name "%name%"
  git config --global user.email %email%

:: Refresh Prompt With \bin\refreshprompt.bat
  refreshprompt
