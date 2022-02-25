@echo off
 ::  File:       addpath.bat
 ::  Author:     Julian Orchard [hello@julianorchard.co.uk]
 ::  Tag Added:  2022-02-24
 ::  Desciption: Add current DIR to PATH

:: Get Args
  set add=%1
  shift

:: Check Args
  if [%add%] == [] ( set add=%cd% )

:: Add Location to PATH
  if exist %add% (
    set PATH="%PATH%;%add%"
  ) else (
    echo Error; the location %add% could not be found.
    echo Please try again.
  )

:: Refresh
  C:\CMD\bin\refresh.bat
