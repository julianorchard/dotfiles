#Persistent
SetTimer, CheckForRL, 30000
CheckForRL:
        if WinExist("Rocket League") and !WinExist("ahk_exe BakkesMod.exe")
        {
                Run, "D:\Programs\Bakkesmod"
        }
        Return
