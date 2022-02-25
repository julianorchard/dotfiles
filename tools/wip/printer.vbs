' Called from Ctrl + o binding in print.ahk
' VBScript is better than Powershell, for this, locally to me...

	Set printlistRead = CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\CMD\ahk\res\printlist.txt", 1)
	Do Until printlistRead.AtEndOfStream
		item = printlistRead.ReadLine
		Set oWsh = CreateObject("Wscript.Shell")
		oWsh.Run """Acrobat.exe"" /p /h" &item,,true
	Loop
	printlistRead.Close
	
' There must be a better way
	Set printlistClear = CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\CMD\ahk\res\printlist.txt", 2)
	printlistClear.Write ""
	printlistClear.Close
