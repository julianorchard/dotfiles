; Random P:\ Image Run

Run C:\Program Files (x86)\Microsoft Office\Office12\EXCEL.EXE "J:\TSD\Xlsm\random-p.xlsm"
WinWait, random-p.xlsm - Microsoft Excel,, 10
If ErrorLevel
  {
    MsgBox, "Couldn't open Excel Document for some reason."
  }

