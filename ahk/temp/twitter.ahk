/* 
File:       twitter.ahk
Author:     Julian Orchard <hello@julianorchard.co.uk>
Tag Added:  2022-03-09
Desciption: Semi-manually looking for handles
*/

#!g::
	Loop
	{
		FileReadLine, line, C:\cmd\ahk\temp\15, %A_Index%
		If ErrorLevel
			Break
		Sleep, 500
		If Ex = 1
		{ 
			Ex = 0
			Break
		}
		MouseClick, Left
		Sleep, 100
		MouseClick, Left
		Sleep, 500
		Send, %line%
		Sleep, 500
		Send, {Enter}
		Sleep, 2000
		KeyWait, g, D
	}
Return
