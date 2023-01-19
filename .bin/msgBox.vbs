''   msgBox.vbs  ---  Send a GUI messagebox.

' Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

'' Description:

' This is mainly called by a few CMD scripts to provide a GUI popup.

'' License:

' See /LICENSE file in the root of this repository.

'' Code:

Set objArgs = WScript.Arguments
messageText = objArgs(0)
MsgBox messageText
