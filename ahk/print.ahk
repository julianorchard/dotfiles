/* 
File:       print.ahk
Author:     Julian Orchard [hello@julianorchard.co.uk]
Tag Added:  2022-02-24
Desciption: Works with Pinter.vbs and Explorer_Get.ahk
 */
;   Ctrl + p to add a file to print list
;   Ctrl + o to print the files in the list

#Include C:\CMD\ahk\res\Explorer_Get.ahk

	^p:: 
		sel := Explorer_GetSelected()
		FileAppend, %sel%`n, C:\CMD\ahk\res\printlist.txt
	; Let user know what's been added
		FileRead, PLContent, C:\CMD\ahk\res\printlist.txt
		selArray := StrSplit(sel, "\")
		MsgBox % "Added '" selArray[selArray.Count()] "'.`n`nThe file printlist.txt currently contains:`n" PLContent
	Return

	^o::
		FileRead, PLContent, C:\CMD\ahk\res\printlist.txt
		if (PLContent = "") 
		{
			MsgBox, 0,, There is no content to print.
			Return
		}
		MsgBox, 4,, File Contents: `n%PLContent%`nWould you like to print?
		IfMsgBox Yes 
			Run, "C:\CMD\tools\printer.vbs"
	;    RIP(rest in powershell)...     Run powershell -ExecutionPolicy bypass -NoLogo -Command "printer.ps1"
	Return
