' Getting Colour, Test
 '	colour = "magick convert C:\cmd\wallpapers\current.jpg +dither -colors 1 -unique-colors txt:"
 '	Const cFini = 1
 '	Const cFail = 2
 '	Set shellExec = shell.Exec(colour), 0, true
 '	WScript.Sleep 10000
 '	Select Case shellExec.Status
 '		Case cFini
 '			colour = shellExec.StdOut.ReadAll
 '		Case cFail 
 '		' Error
 '			colour = "#FFF"
 '	End Select 
 '	WScript.Echo "color = " & colour


