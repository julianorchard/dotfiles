/* 
File:       outlook.ahk
Author:     Julian Orchard [hello@julianorchard.co.uk]
Tag Added:  2022-02-24
Desciption: Terrible Outlook 2007 bindings.
 */

; Cheatsheet: 
;   Ctrl+w      Toggle Read Receipts On/Off
;   Ctrl+o      Start Out Of Office Replies

#IfWinActive, ahk_class rctrl_renwnd32

; Read Receipts On/Off
  ^w::
    Send !t
    Send o
    Send m
    Send !t
    Send r
    PixelGetColor, ischecked, 79, 184
    Sleep, 500
    Send {Enter}
    Send {Escape}
    Send {Escape}
    IfEqual, ischecked, 0xFFFFFF
      TrayTip, Read Receipts On, Outlook read receipts have been enabled.
    Else IfNotEqual, ischecked, 0xFFFFFF
      TrayTip, Read Receipts Off, Outlook read receipts have been disabled.
    Return

; Toggle Out Of Office Assistant
  ^o::
    Send !t
    Send u
    PixelGetColor, oohon, 313, 401
    IfEqual, oohon, 0xFFFFFF
      Send !n
    IfEqual, oohon, 0xFFFFFF  
      TrayTip, Out of Office Assistant Off, Outlook 'auto-replies' have been disabled.
    IfNotEqual, oohon, 0xFFFFFF
      Send !s
    IfNotEqual, oohon, 0xFFFFFF
      TrayTip, Out of Office Assistant On, Outlook 'auto-replies' have been enabled.
    Send {Enter}
    Return

; Disable Out Of Outlook
#IfWinActive
  ^w::
    Return
  ^1::
    Return
  ^o::
    Return
