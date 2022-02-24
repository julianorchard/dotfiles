
' Runs Papetime.vbs

Do 
	WScript.Sleep 30000
	Dim objShell
	Set objShell = Wscript.CreateObject("WScript.Shell")
	objShell.Run "C:\cmd\wallpapers\papetime.vbs" 
Loop While 0 = 0

Set objShell = Nothing


