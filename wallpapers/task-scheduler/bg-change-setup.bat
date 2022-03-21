@echo off

echo Scheduling Background-Changer Task
  schtasks /Create /xml "C:\cmd\wallpapers\task-scheduler\background-changer.xml" /tn "background-changer" /ru %username%
