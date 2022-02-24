; Messy AHK Script to Sync Outlook 2007 
; Calendar with Google Calendar


; Progress Bar / Screen Covering
; Progress, h1500 w2900, Outlook Calendar Syncing. Please do nothing for a little while., Outlook Sync, Syncing Outlook with Google Calendar
  Progress, h500 w900, Outlook Calendar Syncing. Please do nothing for a little while., Outlook Sync, Syncing Outlook with Google Calendar
  ; Export from Outlook
  Run Outlook.exe, , Hide
  ; Wait, and then open 
    Sleep, 3000
    Progress, 10 ; Progress 10%
    Send !f
    Send t
  ; Wait, then select 'Export to a file'
    Sleep, 2000
    Progress, 16.6
    Send {Up}{Up}{Up}{Up}{Up}{Up}
    Send {Down}
    Send {Enter}
  ; Then select 'CSV (Windows)'
    Sleep, 1500
    Progress, 20
    Send {Down}
    Send {Enter}
  ; Then select 'Calendar'
    Sleep, 1500
    Send ca
    Sleep, 2000
    Send ca
    Send {Enter}
    Progress, 25  ; Progress 25
  ; Then type the save file location 
    Sleep, 1500
    Send C:\Users\%A_UserName%\Desktop\temp-cal.csv
    Send {Enter}
    Sleep, 1500
    Send {Enter}
  ; Then tab to enddate and enter a date far in the future
    Send {Tab}
    Send 1/1/2030
    Send {Enter}
  ; Wait 10 seconds, then close
    Sleep, 5000
    Send !q
    Progress, 50  ; Progress 50

  ; Import to Chrome
  Run Chrome.exe
    Sleep, 2000
  ; Navigate to Calendar and enter settings
    Send https://calendar.google.com/calendar/r
    Send {Enter}
    Sleep, 5000
    Send s
    Sleep, 1000
  ; Go to Import/Export Tab
    MouseMove, 82, 540
    Click
    Sleep, 1000
  ; Click File Upload And Locate File On Desktop
    MouseMove, 525, 225
    Click
    Sleep, 1000
    Progress, 75  ; Progress 75
    Send C:\Users\%A_UserName%\Desktop\
    Send {Enter}
    Sleep, 500
    Send +{Tab} 
    Send temp
    Sleep, 500
    Send {Down}
    Sleep, 500
    Send {Enter}
    Sleep, 500
  ; Change to 'Work' Calendar
    Sleep, 500
    Send {Tab}
    Sleep, 500
    Send {Down}
    Sleep, 500
    Send w
    Sleep, 500
    Send {Enter}
    Sleep, 500
  ; Click Import Button
    MouseMove, 453, 352
    Click
    Sleep 2000
    Send !q
    Progress, 100 ; Progress 100


  ; Remove Temp File
  Sleep, 1000
  FileDelete, C:\Users\%A_UserName%\Desktop\temp-cal.csv

  Progress, Off

