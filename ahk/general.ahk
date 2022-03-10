/* 
File:       ahk
Author:     Julian Orchard [hello@julianorchard.co.uk]
Tag Added:  2022-02-24
Desciption: General AutoHotkey Bindings
 */

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                          ;;;
  ;;;   GENERAL AHK BINDINGS   ;;;
  ;;;     - FOR WINDOWS -      ;;;
  ;;;                          ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; File Notes: 
    ; #: win
    ; !: alt  
    ; ^: ctrl
    ; +: shift
    ; *: wildcard
    ; ~: pass through

; General, Simple Bindings
	; Always On Top Window Toggle
		^SPACE::  Winset, Alwaysontop, , A
	; Volume Key; Add Sound Effect
		~Volume_Up::
		~Volume_Down::
			SoundPlay, C:\CMD\ahk\res\bing-1.wav
		Return
	; q to close current window 
		!q::
			WinGetActiveTitle, OutputVar
			WinKill, %OutputVar%
		Return
	; Enter to start custom CMD 
		!Enter:: Run "C:\CMD\cmdrc.bat"
	; Start Enter to Run Git Bash
		#!Enter:: Run "C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Git\Git Bash"
	; Run Firefox with Alt+F
		!f::Run, "C:\Program Files\Mozilla Firefox\firefox.exe"

; Snipping Tool and Drawing
	; Print Screen Opens 'Snipping Tool'
		$PrintScreen::
			Run, "C:\Windows\system32\SnippingTool.exe"
			WinWait, Snipping Tool,, 10
			If ErrorLevel
				{
					MsgBox, "Snipping tool did not open as expected."
				} 
			Else 
				{
					Send, {ctrl down}{n down}
					Send, {ctrl up}{n up}
				}
		Return
	; Alt + U (U for Underline)
	; Draw a straight line from point to point
		!u::
			KeyWait, LButton, D
			MouseGetPos, X1, Y1
			KeyWait, LButton, U
			KeyWait, LButton, D
			MouseGetPos, X2, Y2
			MouseClickDrag, left, X1, Y1, X2, Y2
		Return
	; Alt + B (B for Box)
	; Box in an area with line drawing
		!b::
			KeyWait, LButton, D
			MouseGetPos, X1, Y1
			KeyWait, LButton, U
			KeyWait, LButton, D
			MouseGetPos, X4, Y4
			MouseClickDrag, left, X4, Y1, X1, Y1
			Sleep, 200
			MouseClickDrag, left, X1, Y1, X1, Y4
			Sleep, 200
			MouseClickDrag, left, X1, Y4, X4, Y4
			Sleep, 200
			MouseClickDrag, left, X4, Y1, X4, Y4
		Return

; Line Break / <hr>'s
  ; = Insert  -+-  -+-  -+-  -+-  -+-  -+-  -+-  -+- 
    !=::
      Loop, 8
      {
        Send, {space}-{+}-{space}
      }
    Return
  ; ~ Insert -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    !#::
      Loop, 16 
      { 
        Send, -~ 
      }
      Send, -
    Return
  ; Indented non-markdown-ish "o - "
    !-::
      Send, {space}o{space}-{space}
    Return

; Alt + FX Inserts
  ; F5 Insert Timestamp
    !F5::
      FormatTime, time, A_now, ddd d-MMM-yy hh:mm tt
      send %time%
    Return
  ; F6 Insert Time
    !F6:: 
      FormatTime, time, A_now, ddd d-MMM-yy hh:mm tt
      Send %time% ~ JO : {Enter} 
    Return
  ; Insert a random, serious sign off
    !F7::
      serious := [] 
      lines := 0
      Loop, Read, C:\CMD\ahk\res\serious.txt
      {
        serious.Push(A_LoopReadLine) 
        lines++ 
      }
      Random, randum, 0, %lines%
      Send, % serious[randum]
    Return
  ; Insert a random, silly sign off - X regards, 
    !F8::
      silly := [] 
      lines := 0
      Loop, Read, C:\CMD\ahk\res\silly.txt
      {
        silly.Push(A_LoopReadLine) 
        lines++ 
      }
      Random, randum, 0, %lines%
      Send, % silly[randum]
      Send, {space}regards, 
    Return
  ; Insert a random, LinkedIn Style Message
    !F9::
      linkedin := [] 
      lines := 0
      Loop, Read, C:\CMD\ahk\res\linkedin.txt
      {
        linkedin.Push(A_LoopReadLine) 
        lines++ 
      }
      Random, randum, 0, %lines%
      Send, % linkedin[randum]
    Return

; Insert Lipsum Text
		!F10::
			FileRead, Clipboard, res\lipsum.txt
			Clipwait, 1
			SendInput, %clipboard%
			Clipboard:=""
		Return
; Alt+F12 to hide the taskbar entirely 
	; Edited from; https://www.autohotkey.com/board/topic/83594-how-to-hide-taskbar-with-hotkey/
	; !!! Works only really when the taskbar will autohide in desktop mode !!!
	!F12::
		WinExist("ahk_class Shell_TrayWnd")
		Tog := !Tog
		if (Tog = "1") 
		{
			; This is what I've added...
			SetTimer, CheckForBar, 1000
			CheckForBar:
				if WinExist("ahk_class Shell_TrayWnd") 
				{
					WinHide, ahk_class Shell_TrayWnd
					WinHide, Start ahk_class Button
				}
			Return
		} 
		else 
		{
			SetTimer, CheckForBar, Off ; Also this, ofc
			WinShow, ahk_class Shell_TrayWnd
			WinShow, Start ahk_class Button
		}
	Return

; 'Start+E' On Roids (Alt+E)
  ;  Usage; Alt+E, then c/w/j/p/s/etc.
  ;  opens up folder in Win Explorer
	emodeoff()
	{
		Hotkey, c, C, Off
		Hotkey, w, W, Off
		Hotkey, j, J, Off
		Hotkey, p, P, Off
		Hotkey, s, S, Off
		Hotkey, Esc, EModeEscape, Off
		Progress, Off
	}
	!e::
		Progress, B1 M C0 fs10 ZH0, C = C:\Users\%A_UserName% `n W = C:\Users\%A_UserName%\Documents\Website `n J = J:\TSD\ `n  P = P:\Marketing Images `n S = S:\, Alt+E Locations
		Hotkey, c, C, On
		Hotkey, w, W, On
		Hotkey, j, J, On
		Hotkey, p, P, On
		Hotkey, s, S, On
		Hotkey, Esc, EModeEscape, On
	Return
	C:
		Run, C:\Users\%A_UserName%\
		emodeoff()
	Return
	W:
		Run, C:\Users\%A_UserName%\Documents\Website
		emodeoff()
	Return
	J:
		Run, J:\TSD\
		emodeoff()
	Return
	P:
		Run, P:\
		emodeoff()
	Return
	S:
		Run, S:\
		emodeoff()
	Return
	Esc:
		EModeEscape:
		emodeoff()
	Return

; Screen Refresher and Lock (Alt+I and Alt+L)
	; Alt+I toggles sending RAlt every 15 seconds 
	; keep machine idle, without activating policy
	; enforced screensaver 
			!i:: 
				idltog := !idltog
				if (idltog = "1") 
				{
					TrayTip, Screen Refresher On, The screen refresher has been enabled.
					SetTimer, KeepAlive, 15000
					KeepAlive: 
						Send, {RAlt}
					Return
				}
				else
				{
					TrayTip, Screen Refresher Off, The screen refresher has been disabled.
					SetTimer, KeepAlive, Off ; Turn off the timer
				}
			Return
	; Disable All Keys Alt+L
	; https://www.autohotkey.com/boards/viewtopic.php?t=33925
		lock(f=0, mouse=0, message:="🔒") { 
			static allkeys, ExcludeKeys:="LButton,RButton"
			if !allkeys
			{
				s:="||NumpadEnter|Home|End|PgUp|PgDn|Left|Right|Up|Down|Del|Ins|"
				Loop, 254
					k:=GetKeyName(Format("VK{:0X}",A_Index))
					, s.=InStr(s, "|" k "|") ? "" : k "|"
				For k,v in {Control:"Ctrl",Escape:"Esc"}
				s:=StrReplace(s, k, v)
				allkeys:=Trim(s, "|")
			}
			f:=f ? "On":"Off"
			If mouse
				ExcludeKeys:=""
			For k,v in StrSplit(allkeys,"|")
				If v not in %ExcludeKeys%
					Hotkey, *%v%, Block_Input, %f% UseErrorLevel
				Block_Input:
					If message!=
						Progress, B1 M fs30 ZH0 W50 CTB57EB7 CW0d455e, %message%
					If (f="off")
						Progress, Off
				Return
		}
		!l::
			locktog := !locktog
			if (locktog = "1") 
			{
				lock(1,1)
			}
			else
			{
				lock(0)
			}
		Return

