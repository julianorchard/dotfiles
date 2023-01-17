#Include C:\cmd\ahk\res\Explorer_Get.ahk

^+p::
sel := Explorer_GetSelected()
FileAppend, %sel%`n, C:\cmd\ahk\res\printlist.txt
; Let user know what's been added
FileRead, PLContent, C:\cmd\ahk\res\printlist.txt
selArray := StrSplit(sel, "\")
MsgBox % "Added '" selArray[selArray.Count()] "'.`n`nThe file printlist.txt currently contains:`n" PLContent
Return

^+o::
FileRead, PLContent, C:\cmd\ahk\res\printlist.txt
if (PLContent = "")
{
        MsgBox, 0,, There is no content to print.
        Return
}
MsgBox, 4,, File Contents: `n%PLContent%`nWould you like to print?
IfMsgBox Yes
        Run, "C:\cmd\bin\spooler.bat"
Return
