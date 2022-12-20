$userEnv     = $ENV:UserProfile
$appData     = "\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup"
$startupPath = "$userEnv$appData"
Set-Shortcut -LinkPath "$startupPath\general.ahk.lnk" -TargetPath "C:\cmd\ahk\general.ahk"
Set-Shortcut -LinkPath "$startupPath\spooler.ahk.lnk" -TargetPath "C:\cmd\ahk\spooler.ahk"
Set-Shortcut -LinkPath "$startupPath\printer.ahk.lnk" -TargetPath "C:\cmd\ahk\printer.ahk"
