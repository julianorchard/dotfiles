/* 
File:       bakkesmod.ahk
Author:     Julian Orchard [hello@julianorchard.co.uk]
Tag Added:  2022-02-24
Desciption: Run Bakkesmod when Rocket League opens
 */
#Persistent
; Check Every Half Minute
  SetTimer, CheckForRL, 30000
; Open If RL
  CheckForRL:
  if WinExist("Rocket League") and !WinExist("ahk_exe BakkesMod.exe")
  {
<<<<<<< Updated upstream
    Run, "D:\Programs\Bakkesmod"
=======
    
    Run, "C:\Program Files\BakkesMod\BakkesMod.exe"
>>>>>>> Stashed changes
  }
  Return
