' File:       msgBox.vbs
' Author:     Julian Orchard [hello@julianorchard.co.uk]
' Tag Added:  2022-02-24
' Desciption: Just send a message box (usually called from Batch)
	Set objArgs = WScript.Arguments
	messageText = objArgs(0)
	MsgBox messageText
