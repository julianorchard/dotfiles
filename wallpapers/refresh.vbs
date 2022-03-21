' File:       refresh.vbs
' Author:     Julian Orchard <hello@julianorchard.co.uk>
' Tag Added:  2022-03-21
' Desciption: Runs pape-set.vbs, called from a Task Scheduler task on startup.
  Do 
    ' Every 30 seconds
    WScript.Sleep 30000
    Set objShell = Wscript.CreateObject("WScript.Shell")
    objShell.Run "C:\cmd\wallpapers\bin\pape-set.vbs" 
  Loop While 0 = 0
  Set objShell = Nothing
