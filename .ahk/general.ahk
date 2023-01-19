#Requires AutoHotkey v2.0+
DetectHiddenWindows true

; Home Path
global UserHome := "C:\Users\" A_UserName

; "Alt + Spacebar", Toggles Window 'Always on top' status
^SPACE::WinsetAlwaysOnTop -1, WinGetTitle("A")

; Add Sound to Volume Keys
~Volume_Up::
~Volume_Down::SoundPlay("C:\Windows\Media\Windows Background.wav")

; "Alt + q", to close current window
!q::WinKill(WinGetTitle("A"))

; "Alt + Enter", to run CMDrc
$!Enter::
{
  if not WinActive("ahk_exe EXCEL.EXE")
  {
    Run(UserHome "\cmdrc.bat")
  }
  else
  {
    Send("!{enter}")
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

;; Function to return the coordinates of two points.
;; Used by !u::, underline, and !b::, box drawing
;; hotkeys, mainly alongside Snipping Tool
GetDrawingCoords()
{
  KeyWait("LButton", "D")
  MouseGetPos(&xa, &ya)

  KeyWait("LButton", "U")
  KeyWait("LButton", "D")
  MouseGetPos(&xb, &yb)

  ;; We reverse this here because we want to
  ;; draw the lines from the point the cursor
  ;; currently is, so b coords first (as they're
  ;; the second set) and then the a ones.
  c := [xb, yb, xa, ya]
  Return c
}

; Alt + U (U for Underline)
!u::
{
  c := GetDrawingCoords()
  MouseClickDrag("L", c[1], c[2], c[3], c[4])
  MouseClickDrag("L", c[3], c[4], c[1], c[2])
}

; Alt + B (B for Box)
!b::
{
  ;; Although this doesn't matter which
  ;; points you click first, this is the
  ;; basic visualisation:

  ;;    x1,y1 --------------- x2,y1
  ;;      |                     |
  ;;      |                     |
  ;;      |                     |
  ;;      |                     |
  ;;    x1,y2 --------------- x2,y2

  c := GetDrawingCoords()
  x1 := c[1]
  y1 := c[2]
  x2 := c[3]
  y2 := c[4]

  ;; TODO:
  ;; I would like to do this in a loop,
  ;; but the MouseClickDrag doesn't want
  ;; to do that... might be able to use
  ;; something else instead.
  MouseClickDrag("L", x1, y1, x2, y1)
  Sleep(200)
  MouseClickDrag("L", x2, y1, x2, y2)
  Sleep(200)
  MouseClickDrag("L", x2, y2, x1, y2)
  Sleep(200)
  MouseClickDrag("L", x1, y2, x1, y1)
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
  textToInsert := FileSelect(3, UserHome "\org\txt", "Insert Email Template")
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
global taskbarStatus := false
$!F12::
{
  global taskbarStatus
  HideShowTaskbar(taskbarStatus := !taskbarStatus)
}
HideShowTaskbar(status)
{
  if (status)
  {
    ;; Set timer
    SetTimer(FixTaskbarHide, 500)
  }
  else
  {
    ;; Unset timer and show window
    SetTimer(FixTaskbarHide, 0)
    WinShow("ahk_class Shell_TrayWnd")
  }
}
FixTaskbarHide()
{
  ;; Check if the window is visible, using DllCall
  if DllCall("IsWindowVisible", "Ptr", WinExist("ahk_class Shell_TrayWnd"))
  {
    ;; If it is, hide it
    WinHide("ahk_class Shell_TrayWnd")
  }
}

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
  Return ih.EndKey
}
!e::
{
  ;; It would be nicer to be able to do this with
  ;; an array or something; make it easier to add
  ;; new items to both the Gui and also to the
  ;; script part...

  ;; GUI element
  AltEGui := Gui()
  AltEGui.Opt("-Caption")
  AltEGui.MarginX := 100
  AltEGui.MarginY := 30
  AltEGui.SetFont("s12", "Segoe UI")
  AltEGui.Add("Text", , "Alt+E locations you can open (bindings below):")
  AltEGui.SetFont("s10", "Consolas")
  AltEGui.Add("Text", , "- binding:    C   =     " UserHome)
  AltEGui.Add("Text", , "- binding:    W   =     " UserHome "\Documents\Website\")
  AltEGui.Add("Text", , "- binding:    J   =     J:\TSD\")
  AltEGui.Add("Text", , "- binding:    P   =     P:\Marketing Images\")
  AltEGui.Add("Text", , "- binding:    S   =     S:\")
  AltEGui.Show

  Switch KeyWaitAny()
  {
    case "c":
      Run(UserHome "\")
    case "w":
      Run(UserHome "\Documents\Website")
    case "j":
      Run("J:\TSD\")
    case "p":
      Run("P:\")
    case "s":
      Run("S:\")
    default:
  }
  AltEGui.Destroy
}


;; Alt + i : Toggle screen refresher
Refresher()
{
  Send("{RAlt}")
}
global refresherStatus := false
!i::
{
  global refresherStatus
  refresherStatus := !refresherStatus
  if (refresherStatus)
  {
    TrayTip("Screen Refresher On", "The screen refresher has been enabled.")
    SetTimer(Refresher, 15000)
  }
  else
  {
    TrayTip("Screen Refresher Off", "The screen refresher has been disabled.")
    SetTimer(Refresher, 0)
  }
}

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
