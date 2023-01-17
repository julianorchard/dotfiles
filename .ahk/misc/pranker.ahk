;;   pranker.ahk  ---  Does some tomfoolery when in use.

; Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

;; Description:

; Slightly inspired by mathiasbynens/evil.sh, but for people
; who have AutoHotkey installed.

;; License:

; See /LICENSE file in the root of this repository.

;; Code:

; File:  "pranker.ahk"

#NoTrayIcon
#Persistent

; I do like the new system... >:)
  SetTitleMatchMode, 3
  SetTimer, iDoLikeTheNewSystem, 2000
  iDoLikeTheNewSystem:
    IfWinExist, Sign In
    {
      WinClose, Sign In
    }
  Return

; Decides approx level of randomness
  veryhigh:=5
  high:=30
  medium:=50
  low:=100
  verylow:=500

; Sends random q instead of w
  $w::
    Random, rng, 1, high
    If (rng == 1) {
      Send, q
    } Else {
      Send, w
    }
    Return

; Sends random mn rather than just m
  $m::
    Random, rng, 1, high
    If (rng == 1) {
      Send, mn
    } Else {
      Send, m
    }
    Return

; Sends random 'LWin' (start key) when
; z is pressed!
  $z::
    Random, rng, 1, medium
    If (rng == 1) {
      Send, {LWin}
    } Else {
      Send, z
    }
    Return

; Sends randomn number from 3 to 7,
; when 5 is pressed!
  $5::
    Random, rng, 1, medium
    Random, sender, 3, 7
    If (rng == 1) {
      Send, %sender%
    } Else {
      Send, 5
    }
    Return

; Occasionally send left click instead of right click
  $RButton::
    Random, rng, 1, high
    If (rng == 1) {
      Click
    } Else {
      Click, Right
    }
    Return

; Quite frequently toggle the numlock on 'spacebar' press
  $Space::
    Random, rng, 1, veryhigh
    If (rng == 1) {
      Send, {Space}
      Send, {NumLock}
    } Else {
      Send, {Space}
    }
    Return

; Occasionally send Ctrl+A, Backspace when
; intending Ctrl+Backspace
  $^Backspace::
    Random, rng, 1, verylow
    If (rng == 1) {
      Send, ^{a}
      Send, {Backspace}
    } Else {
      Send, ^{Backspace}
    }
    Return

; Swap scroll direction, and amount
  $WheelDown::
    Random, rng, 1, medium
    If (rng == 1) {
      Send, {WheelUp 2}
    } Else If (rng == 2) {
      Send, {WheelDown 5}
    } Else {
      Send, {WheelDown}
    }
    Return
  $WheelUp::
    Random, rng, 1, medium
    If (rng == 1) {
      Send, {WheelDown 2}
    } Else If (rng == 2) {
      Send, {WheelUp 5}
    } Else {
      Send, {WheelUp}
    }
    Return

;; Can't get this to work properly
;; Sends random 'esc', 'n' when 'Ctrl+Enter'
;; (so that, when using Outlook 2007, the email
;; that was intending to be sent gets closed...!)
;   ^Enter::
;     Random, rng, 1, veryhigh
;     If (rng == 1) && WinActive("ahk_exe OUTLOOK.exe") {
;       Send, {esc}
;       Send, n
; ; The 'n' means it doesn't save the email being composed...!
;     } Else {
;       Send, {Ctrl Down}
;       Send, {Enter}
;       Send, {Ctrl Up}
;     }
;     Return

