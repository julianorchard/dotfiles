file=\Users\jorchard\Documents\Applications\kaomoji\resources\descriptions-and-kaomoji.txt
InputBox, SearchString, Search in %file%, Entering the string to search
Line := False
Loop, Read, %file%
{
	If !Trim(A_LoopReadLine)
		Continue
	If InStr(A_LoopReadLine, SearchString)
	{
		Line := A_LoopReadLine
		Continue ;second concerned line
	}
	If Line
	{
		Line .= "`r`n" . A_LoopReadLine
		Break
	}
}
MsgBox, % Line
