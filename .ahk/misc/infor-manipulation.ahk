#Requires AutoHotkey v2.0+

;; File:        infor-bcp.ahk
;; Author:      Julian Orchard <hello@julianorchard.co.uk>
;; Tag Added:   2023-02-08
;; Description: Helper scripts for using the Infor System.

;; Quick task changing Stock Item location for MH
!#0::
{
  Loop {
    Send("{F4}")
    ;; Move mouse to box and await F4
    Sleep(2000)
    MouseMove(664, 190)
    Click
    KeyWait("F4", "D")
    Sleep(1000)

    ;; Move mouse to Buyer and input
    MouseMove(721, 706)
    Click
    Send("Build Kit Items{enter}")
    Sleep(500)

    ;; Send F4 to save menu it
    Send("^{s}")
  }

}

Escape::ExitApp
