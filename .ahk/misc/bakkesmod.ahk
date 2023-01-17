#Requires AutoHotkey v2.0+

; [2023-01-19] NOTE  --- Julian <jorchard@pm.me>
; I don't really use this. It's not accurate enough, either: it
; accidentally picks up browser windows with Rocket League in, too.

Persistent 1

SetTimer CheckForRL, 30000

CheckForRL()
{
    if WinExist("Rocket League") and !WinExist("ahk_exe BakkesMod.exe")
    {
        Run, "D:\Programs\Bakkesmod"
    }
}
