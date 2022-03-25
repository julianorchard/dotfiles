@echo off

::  File:       pape-set-task.bat
::  Author:     Julian Orchard <hello@julianorchard.co.uk>
::  Tag Added:  2022-03-24
::  Desciption: Sets the Task Scheduler 'on-startup' task.

echo Scheduling Background-Changer Task
  schtasks /Create /xml "C:\cmd\wallpapers\task-scheduler\background-changer.xml" /tn "background-changer" /ru %username%
