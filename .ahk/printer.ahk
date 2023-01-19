#Requires AutoHotkey v2.0+

Persistent 1

global printNumber
printNumber := FileRead(A_ScriptDir "\printer_number.txt")
SetTimer CheckForPrintingWindow, 250

CheckForPrintingWindow()
{
  if WinActive("Store Details")
  {
    Send("{tab}")
    Sleep(250)
    Send(printNumber)
    Sleep(250)
    Send("{enter}")
  }
}

