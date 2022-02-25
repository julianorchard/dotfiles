@echo off

:: Setting Up All 'Routine' Tasks
  set routineFolder=C:\CMD\routine

:: Screen Reminder Task; hourly
  echo SCREEN REMINDER TASK
    schtasks /Create /tn "Screen Reminder" /tr "%routineFolder%\take-a-break.vbs" /sc hourly /st 00:00:00 /sd 01/01/2010 /ru %username%

:: Outlook Calendar Sync; daily
  echo OUTLOOK 2007 CALENDAR SYNC TASK
    schtasks /Create /xml "%routineFolder%\outlook-sync\CalendarSync.xml" /tn "Cal Sync" /ru %username%

:: Random Image Task; daily
  echo RANDOM IMAGE EMAIL TASK
    schtasks /Create /tn "Random P" /tr "%routineFolder%\random-p\random-p.ahk" /sc weekly /st 09:00:00 /sd 19/08/2021 /ru %username%

