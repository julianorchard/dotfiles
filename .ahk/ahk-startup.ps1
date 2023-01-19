$userEnv     = $ENV:UserProfile
$appData     = "\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup"
$startupPath = "$userEnv$appData"

Set-Shortcut -LinkPath "$startupPath\general.ahk.lnk" -TargetPath "$userEnv\.ahk\general.ahk"
Set-Shortcut -LinkPath "$startupPath\printer.ahk.lnk" -TargetPath "$userEnv\.ahk\printer.ahk"
