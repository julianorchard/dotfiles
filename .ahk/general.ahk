#Requires AutoHotkey v2.0+

; Home Path
; HOME := "C:\Users\%A_UserName%"

; "Alt + Spacebar", Toggles Window 'Always on top' status
^SPACE::WinsetAlwaysOnTop -1, WinGetTitle("A")

; Add Sound to Volume Keys
~Volume_Up::
~Volume_Down::SoundPlay("C:\Windows\Media\Windows Background.wav")

; "Alt + q", to close current window
!q::WinKill(WinGetTitle("A"))

; "Alt + Enter", to run CMDrc
!Enter::
{
  if not WinActive("ahk_class Microsoft Excel")
  {
    Run("C:\Users\jorchard\cmdrc.bat")
  }
}


; "Win + Enter", to run Powershell
#Enter::Run("C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe")

; "Win + Alt + Enter", to Run Git Bash
#!Enter::Run("C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Git\Git Bash")

; "Alt + F", to Run Firefox
!f::Run("C:\Program Files\Mozilla Firefox\firefox.exe")

; "Alt + c", to get "MouseMove, X, Y" positions to the clipboard
!c::
{
  MouseGetPos(&xpos, &ypos)
  A_Clipboard := "MouseMove(" xpos ",  " ypos ")"
}

;; Snipping Tools
$PrintScreen::
{
  Run("C:\Windows\system32\SnippingTool.exe")
  if not WinWait("Snipping Tool", , 5)
  {
    MsgBox "Snipping tool timed out."
  }
  else
  {
    Send("^n")
  }
}

; Alt + U (U for Underline)
!u::
{
  KeyWait("LButton", "D")
  MouseGetPos(&X1, &Y1)

  KeyWait("LButton", "U")
  KeyWait("LButton", "D")
  MouseGetPos(&X2, &Y2)

  MouseClickDrag("L", X1, Y1, X2, Y2)
  MouseClickDrag("L", X2, Y2, X1, Y1)
}

; Alt + B (B for Box)
!b::
{
  KeyWait("LButton", "D")
  MouseGetPos(&X1, &Y1)
  KeyWait("LButton", "U")
  KeyWait("LButton", "D")
  MouseGetPos(&X4, &Y4)
  MouseClickDrag("L", X4, Y4, X4, Y1)
  Sleep(200)
  MouseClickDrag("L", X4, Y1, X1, Y1)
  Sleep(200)
  MouseClickDrag("L", X1, Y1, X1, Y4)
  Sleep(200)
  MouseClickDrag("L", X1, Y4, X4, Y4)
}

; TODO: Try making a solid fill box, could be interesting

; Line Break / <hr>'s - - - - - - - - - - - - - - -

; = Insert  -+-  -+-  -+-  -+-  -+-  -+-  -+-  -+-
!=::
{
  Loop(8)
  {
    Send("{space}-{+}-{space}")
  }
}

; ~ Insert -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
!#::
{
  Loop(16)
  {
    Send("-~")
  }
  Send("-")
}

; Indented non-markdown-ish "o - "
!-::Send("{space}o{space}-{space}")

; Instagram Hashtags
#!i::Send(FileRead("res\wt.txt") "{backspace 2}")

; F5 Insert Timestamp
!F5::Send(FormatTime(, "ddd d-MMM-yy hh:mm tt"))

; F6 Insert Time
!F6::Send(FormatTime(, "ddd d-MMM-yy hh:mm tt") " ~ JO : {enter}")

; F7/F8: SignOff function inserts a random
; Email signoff from an input file.
RandomFromFile(file)
{
  serious := []
  lines := 0
  Loop read, file
  {
    serious.Push(A_LoopReadLine)
    lines++
  }
  randomNumber := Random(0, lines)
  Return serious[randomNumber]
}

; F7 Serious
!F7::Send(RandomFromFile("res\serious.txt"))
; F8 Silly
!F8::Send(RandomFromFile("res\silly.txt") " regards, ")

; F11 Select an email template to insert
!F11::
{
  textToInsert := FileSelect(3, "C:\Users\jorchard\org\txt", "Insert Email Template")
  if !(textToInsert = "")
  {
    Loop read, textToInsert
    {
      Send(A_LoopReadLine "{Enter}")
    }
  }
}

; F9 Insert a random, LinkedIn Style Message
!F9::Send(RandomFromFile("res\linkedin.txt"))

; Insert Lipsum Text
!F10::Send(FileRead("res\lipsum.txt") "{backspace 2}")

; Alt+F12 to hide the taskbar entirely
; CheckForTaskbar() {
;   if WinExist("ahk_class Shell_TrayWnd")
;   {
;     WinHide("ahk_class Shell_TrayWnd")
;     WinHide("Start ahk_class Button")
;   }
; }
; !F12::
; {
;   if (WinExist("ahk_class Shell_TrayWnd"))
;   {
;     SetTimer CheckForTaskbar, 1000
;   }
;   else
;   {
;     SetTimer CheckForTaskbar, , 0
;     WinShow("ahk_class Shell_TrayWnd")
;     WinShow("Start ahk_class Button")
;   }
; }


;; Alt + e, Start + e improved
KeyWaitAny(Options:="")
{
    ih := InputHook(Options)
    if !InStr(Options, "V")
    {
        ih.VisibleNonText := false
    }
    ih.KeyOpt("{All}", "E")
    ih.Start()
    ih.Wait()
    return ih.EndKey
}
!e::
{
  ;; It would be nicer to be able to do this with
  ;; an array or something; make it easier to add
  ;; new items to both the Gui and also to the
  ;; script part...

  ;; GUI element
  MyGui := Gui()
  MyGui.Opt("-Caption")
  MyGui.MarginX := 100
  MyGui.MarginY := 30
  MyGui.SetFont("s12", "Segoe UI")
  MyGui.Add("Text", , "Alt+E locations you can open (bindings below):")
  MyGui.SetFont("s10", "Consolas")
  MyGui.Add("Text", , "- binding:    C   =     C:\Users\%A_UserName%\")
  MyGui.Add("Text", , "- binding:    W   =     C:\Users\%A_UserName%\Documents\Website\")
  MyGui.Add("Text", , "- binding:    J   =     J:\TSD\")
  MyGui.Add("Text", , "- binding:    P   =     P:\Marketing Images\")
  MyGui.Add("Text", , "- binding:    S   =     S:\")
  MyGui.Show

  Switch KeyWaitAny()
  {
    case "c":
      Run("C:\Users\jorchard\")
    case "w":
      Run("C:\Users\jorchard\Documents\Website")
    case "j":
      Run("J:\TSD\")
    case "p":
      Run("P:\")
    case "s":
      Run("S:\")
    default:
  }
  MyGui.Destroy
}

; !i::
; idltog := !idltog
; If (idltog = "1")
; {
;         TrayTip, Screen Refresher On, The screen refresher has been enabled.
;         SetTimer, KeepAlive, 15000
; KeepAlive:
;         Send, {RAlt}
;         Return
; }
; Else
; {
;         TrayTip, Screen Refresher Off, The screen refresher has been disabled.
;         SetTimer, KeepAlive, Off ; Turn off the timer
; }
; Return

; lock(f=0, mouse=0, message:="🔒") {
;         static allkeys, ExcludeKeys:=""LButton",RButton"
;         If !allkeys
;         {
;                 s:="||NumpadEnter|Home|End|PgUp|PgDn|Left|Right|Up|Down|Del|Ins|"
;                 Loop, 254
;                         k:=GetKeyName(Format("VK{:0X}",A_Index))
;                 , s.=InStr(s, "|" k "|") ? "" : k "|"
;                 For k,v in {Control:"Ctrl",Escape:"Esc"}
;                 s:=StrReplace(s, k, v)
;                 allkeys:=Trim(s, "|")
;         }
;         f:=f ? "On":"Off"
;         If mouse
;                 ExcludeKeys:=""
;         For k,v in StrSplit(allkeys,"|")
;         If v not in %ExcludeKeys%
;                 Hotkey, *%v%, Block_Input, %f% UseErrorLevel
; Block_Input:
;         If message!=
;                 Progress, B1 M fs30 ZH0 W50 CTB57EB7 CW0d455e, %message%
;         If (f="off")
;                 Progress, Off
;         Return
; }
; !l::
; locktog := !locktog
; If (locktog = "1")
; {
;         lock(1,1)
; }
; Else
; {
;         lock(0)
; }
; Return

;; Start + Alt + a: Move items from last month
;; into my '! Urgent" folder in Outlook 2007.
!#a::
{
  ;; Pain. I upgraded from ahk 1.1 to 2.0 to access this
  ;; feature, below, that wasn't even required.
  ; twoWeeksAgo := DateAdd(A_Now, -14, "days")
  ; MsgBox FormatTime(twoWeeksAgo, "yyyy-MM-dd")

  ;; Sort the view by 'last month' and go back to
  ;; focus on email list of items
  send("+{tab}{enter}{tab 10}{down 8}{enter}{tab 2}")
  sleep(1000)

  ;; Move all items to '! Urgent' folder (alt+e, m to move)
  send("^{a}")
  send("!em{!}{enter}")

  send("+{tab 14}")
  sleep(1000)
  send("{enter}{escape}")
}
