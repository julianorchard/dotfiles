; Fixing (By Ignoring) An Issue I Didn't
; Have Elevation To Fix At Work 

#Persistent 

  SetTimer, CheckWin, 500

  CheckWin:
  IfWinExist, AdobeGCClient.exe - Bad Image
  { 
    Send {enter}
  }
  IfWinExist, FullTrustNotifier.exe - Bad Image
  {
    Send {enter}
  }
  Return
