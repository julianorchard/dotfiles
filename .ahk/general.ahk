; "Alt + Spacebar", Toggles Window 'Always on top' status
^SPACE::  Winset, Alwaysontop, , A

; Add Sound to Volume Keys
~Volume_Up::
~Volume_Down::
SoundPlay, C:\cmd\ahk\res\bing-1.wav
Return

; "Alt + q", to close current window
!q::
WinGetActiveTitle, OutputVar
WinKill, %OutputVar%
Return

; "Alt + Enter", to run CMDrc
#IfWinNotActive, ahk_class Microsoft Excel
!Enter:: Run, "C:\cmd\cmdrc.bat"

; "Win + Enter", to run Powershell
#Enter:: Run, "C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe"

; "Win + Alt + Enter", to Run Git Bash
#!Enter:: Run, "C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Git\Git Bash"

; "Alt + F", to Run Firefox
!f::Run, "C:\Program Files\Mozilla Firefox\firefox.exe"

; "Alt + c", to get "MouseMove, X, Y" positions to the clipboard
!c::
MouseGetPos, xpos, ypos
Clipboard := "MouseMove, " xpos ",  " ypos 
Return

;; Snipping Tools
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
!u::
KeyWait, LButton, D
MouseGetPos, X1, Y1
KeyWait, LButton, U
KeyWait, LButton, D
MouseGetPos, X2, Y2
MouseClickDrag, left, X1, Y1, X2, Y2
Return

; Alt + B (B for Box)
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

; Line Break / <hr>'s - - - - - - - - - - - - - - -

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

#!i::
FileRead, Clipboard, C:\cmd\ahk\res\wt.txt
Clipwait, 1
Send, %clipboard%
Clipboard:=""
Return

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
Loop, Read, C:\cmd\ahk\res\serious.txt
{
        serious.Push(A_LoopReadLine)
        lines++
}
Random, randum, 0, %lines%
Send, % serious[randum]
Return

; Insert a random, non-serious sign off - X regards,
!F8::
silly := []
lines := 0
Loop, Read, C:\cmd\ahk\res\silly.txt
{
        silly.Push(A_LoopReadLine)
        lines++
}
Random, randum, 0, %lines%
Send, % silly[randum]
Send, {space}regards,
Return

!F11::

FileSelectFile, text_to_insert, 3, C:\Users\%A_UserName%\org\txt, Insert Email Template

If !(text_to_insert = "")
{
        Loop, Read, %text_to_insert%
        {
                Send, % A_LoopReadLine "{Enter}"
        }
}

Return

; Insert a random, LinkedIn Style Message
!F9::
linkedin := []
lines := 0
Loop, Read, C:\cmd\ahk\res\linkedin.txt
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
!F12::
WinExist("ahk_class Shell_TrayWnd")
Tog := !Tog
If (Tog = "1")
{
        SetTimer, CheckForBar, 1000
CheckForBar:
        If WinExist("ahk_class Shell_TrayWnd")
        {
                WinHide, ahk_class Shell_TrayWnd
                WinHide, Start ahk_class Button
        }
        Return
}
Else
{
        SetTimer, CheckForBar, Off
        WinShow, ahk_class Shell_TrayWnd
        WinShow, Start ahk_class Button
}
Return

emodeoff()
{
        Hotkey, c, C, Off
        Hotkey, w, W, Off
        Hotkey, j, J, Off
        Hotkey, p, P, Off
        Hotkey, s, S, Off
        Hotkey, Esc, EModeEscape, Off

        Gui, Destroy
}
!e::
Gui, -Caption
Gui, Margin, 100, 30
Gui, Font, s12, Segoe UI
Gui, Add, Text,, Alt+E locations you can open (bindings below):
Gui, Font, s10, Consolas
Gui, Add, Text,, - binding:    C   =     C:\Users\%A_UserName%\
Gui, Add, Text,, - binding:    W   =     C:\Users\%A_UserName%\Documents\Website\
Gui, Add, Text,, - binding:    J   =     J:\TSD\
Gui, Add, Text,, - binding:    P   =     P:\Marketing Images\
Gui, Add, Text,, - binding:    S   =     S:\
Gui, Show
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

!i::
idltog := !idltog
If (idltog = "1")
{
        TrayTip, Screen Refresher On, The screen refresher has been enabled.
        SetTimer, KeepAlive, 15000
KeepAlive:
        Send, {RAlt}
        Return
}
Else
{
        TrayTip, Screen Refresher Off, The screen refresher has been disabled.
        SetTimer, KeepAlive, Off ; Turn off the timer
}
Return

lock(f=0, mouse=0, message:="🔒") {
        static allkeys, ExcludeKeys:="LButton,RButton"
        If !allkeys
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
If (locktog = "1")
{
        lock(1,1)
}
Else
{
        lock(0)
}
Return

$^#l::
SetStoreCapsLockMode, off
a := "{NumLock}"
b := "{CapsLock}"
c := "{ScrollLock}"
SetNumLockState, off
SetCapsLockState, off
SetScrollLockState, off
ql := 1000
Loop, 8
{
  Send, %a%
  Sleep, %ql%
  Send, %a%
  Send, %b%
  Sleep, %ql%
  Send, %b%
  Send, %c%
  Sleep, %ql%
  Send, %c%
  ql := ql/2
}
Sleep, 200
Send, %a%
Send, %b%
Send, %c%
Sleep, 50
Send, %a%
Send, %b%
Send, %c%
Sleep, 100
Send, %a%
Send, %b%
Send, %c%
Sleep, 50
Send, %a%
Send, %b%
Send, %c%
Return

$^#w::
SetStoreCapsLockMode, off
a := "{NumLock}"
b := "{CapsLock}"
c := "{ScrollLock}"
SetNumLockState, off
SetCapsLockState, off
SetScrollLockState, off
count := 1
Loop, 3
{
  Loop, 3
  {
    Sleep, 100
    Send, %c%
    Sleep, 60
    Send, %c%
    Send, %b%
    Sleep, 60
    Send, %b%
    Send, %a%
    Sleep, 60
    Send, %a%
  }
  If (count == 1)
  {
    Sleep, 200
    Send, %c%
    Sleep, 200
    Send, %c%
    Send, %b%
    Sleep, 200
    Send, %b%
  }
  Else If (count == 2)
  {
    Sleep, 200
    Send, %b%
    Sleep, 200
    Send, %b%
    Send, %a%
    Sleep, 200
    Send, %a%
  }
  Else
  {
    Sleep, 200
    Send, %a%
    Sleep, 200
    Send, %a%
    Send, %b%
    Sleep, 200
    Send, %b%
    Send, %a%
    Sleep, 100
    Send, %b%
    Sleep, 100
    Send, %c%
    Sleep, 800
    Send, %c%
    Sleep, 200
    Send, %b%
    Sleep, 200
    Send, %a%
    Sleep, 400
    Send, %a%
    Send, %b%
    Send, %c%
    Sleep, 400
    Send, %a%
    Send, %b%
    Send, %c%
  }
  count := count+1
}
Return
