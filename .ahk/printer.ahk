#Persistent
FileRead, print_no, %A_ScriptDir%\printer_number.txt
SetTimer, CheckWin, 500
CheckWin:
        IfWinActive, Store Details 
        {
                Send, {Tab}
                Sleep, 500
                Send, %print_no%
                Sleep, 500
                Send, {Enter}
        }
        Return
