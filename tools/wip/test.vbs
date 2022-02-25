set wshshell = wscript.CreateObject("wscript.shell")
wshshell.AppActivate("C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Chrome.exe")
WshShell.SendKeys("Username{tab}")
WshShell.SendKeys("Password{enter}")
