
^!m:: 
Loop, 40 
{ 
	if stopit = 1
	{
		stopit = 0
		break
	}
	Sleep, 1000 
	MouseClick, right
	Sleep, 500
	Send, {down}{down}
	Sleep, 500
	Send, {enter}
	Sleep, 1500
	Send, {enter}
	Sleep, 500
	Send, {right}
} 
return

q::
	MsgBox, 0, ,stopit !, .6
	stopit = 1
return