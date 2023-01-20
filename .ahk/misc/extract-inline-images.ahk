#Requires AutoHotkey v2.0+

;; Extract inline images from emails using the
;; greatest program on Windows: Paint.

!#e::
{
  paint := "ahk_exe mspaint.exe"

  ;; If paint is open, close it (we want a fresh
  ;; document anyway):
  if WinExist(paint)
  {
    WinClose(paint)
  }
  Run(A_WinDir "\System32\mspaint.exe")

  ;; Wait for Paint to open, if not open after 5 seconds,
  ;; notify and exit
  if !WinWait(paint, , 5)
  {
    MsgBox("Paint has taken too long to open.")
    Return
  }

  ;; Send 'Paste' and 'Save' key combinations
  Send("^v")
  Send("^s")

  ;; Wait for Save As window (if not open after 5 seconds,
  ;; notify user and exit the function
  if !WinWait("Save As", , 5)
  {
    MsgBox("'Save As' dialogue has taken too long to open.")
    Return
  }
  else
  {
    ;; Select .jpeg option
    Send("{tab}{down}{up 3}^{tab}")

    ;; Input filename field
    Sleep(100)
    Send("^n")
    Sleep(100)

    ;; Send Desktop as the Save As location
    Send("%HOME%\Desktop\")
  }

  ;; The user can then name the file... I would like to do this automatically,
  ;; of course, but it's more hassle than it's worth.

  ;; End!
}
