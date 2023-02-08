;; File:        infor-manipulation.ahk
;; Author:      Julian Orchard <hello@julianorchard.co.uk>
;; Tag Added:   2022-10-26
;; Description: Helper scripts for using the Infor System.

;; Just reload the script if we hit ESC
~Esc::
  If WinActive("ahk_exe WinStudio.exe") {
    Reload
  }
Return


;; UNUSED: Inputs a load of Stock numbers from a CSV file
;;         to basically marry up a stock number with its
;;         location within the system
#!0::
  csv_in_file := "C:\Users\jorchard\testing-in.csv"
  csv_out_file := "C:\Users\jorchard\testing-out.csv"

  Loop, read, %csv_in_file%
  {
    Loop, parse, A_LoopReadLine, %A_Tab%
    {
      ;; Now in: WEX_Items
      Send, {F4}
      Sleep, 2000
      MouseMove, 722, 218
      Click
      Send, {Ctrl Down}{Shift Down}{Left 3}{Ctrl Up}{Shift Up}
      Send, %A_LoopField%{F4}
      Sleep, 2000

      ;; Now in: Item Stockroom Locations
      Send, ^{Tab}
      Sleep, 2000

      MouseMove, 640, 396
      Click
      Sleep, 500
      clipboard := ""
      Send, ^c
      ClipWait

      ;; Use %clipboard% to append the current line of file
      new_file_text = "%clipboard%,%A_LoopField%`n"
      FileAppend, %new_file_text%, "%csv_out_file%"

      ;; Now in: WEX_Items
      Send, ^{Tab}
      Sleep, 2000
    }
  }
Return


;; Completes the Qty Adjustments Infor Screen
;; zeroing the value in 'STOCK':
#!1::
  MouseMove, 474, 369
  Click
  Send, 0
  MouseMove, 469, 392
  Click
  MouseMove, 474, 454
  Click
  MouseMove, 456, 470
  Click
  Send, STOCK
  MouseMove, 471, 528
  Click
  Send, SC
Return


;; Helps complete a Qty Move sheet, waiting for
;; user input at the right moments, too!
#!2::
;; Item Number Input
  MouseMove, 662,  252
  Click
  KeyWait, Tab, D
  Sleep, 500

;; Location Number Input
  MouseMove, 601,  443
  Click
  KeyWait, LButton, D
  Click

;; Quantity Input
  MouseMove, 551,  405
  Click
  KeyWait, Tab, D

;; To Location
  MouseMove, 973,  440
  Click
  KeyWait, LButton, D
  Click

;; Sort problem with SC input
  MouseMove, 902,  739
  Click
  Click

;; TODO: This bit is SUPER unreliable ------
;; Document Number (SC)
  MouseMove, 909,  521
  Click
  Send, SC
;; END TODO --------------------------------

;; Enable Process Button
  MouseMove, 902,  739
  Click

  KeyWait, LButton, D
;; This Clicks the Process Button (bad idea...)
  MouseMove, 1090,  309
  ; Click
  Sleep, 500
  Send, {Enter}

Return

