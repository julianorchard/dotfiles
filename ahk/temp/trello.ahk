; Adding some Trello bindings

; For deleting cards from
; the archived items list
#IfWinActive, ahk_class Chrome_WidgetWin_1
#!a::
Loop, 5 {
; Open Card
  MouseMove, 1750, 260
  Sleep, 500
  Click
; Delete Button
  MouseMove, 1250, 950
  Sleep, 750
  Click
; Confirm Delete
  MouseMove, 1250, 900
  Sleep, 250
  Click
}
Return
