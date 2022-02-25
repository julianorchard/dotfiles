
' Clock.vbs         ~~ Julian Orchard [hello@julianorchard.co.uk]
'   This file handles the bulk of the writing
'   content to the file, check.vbs handles the
'   reading more than this.

' Loop through the log file, copying each line
' of the file, unless it's the line we want to change
' in which case we just replace that line with the new
' content and move on; then replace the file at the end
	Function AddTime(ByVal inOut, ByVal currentTime, ByVal searchDate)

	' Get File Objects
 	  Set fso      = CreateObject("Scripting.FileSystemObject")
	  Set editFile = fso.OpenTextFile("C:\CMD\hours\log\log-temp.csv",2,True)
    Set readFile = fso.OpenTextFile("C:\CMD\hours\log\log.csv",1)

	' Loop Through File
	  Do While readFile.AtEndOfStream <> True

    ' Current Line To Array
      lineFull = readFile.ReadLine
	    lineArray = Split(lineFull, ",")
	    lineDate  = DateValue(lineArray(0))

    ' Found The Input Date (seachDate)
	    If lineDate = searchDate Then
        If inOut = "in" Then
      ' For Replacing In
          WScript.Echo "Replacing 'in' time " & lineArray (1) & " with " & currentTime & "."
          editFile.WriteLine lineArray(0) & "," & _
                             currentTime      & "," & _
                             lineArray(2)
        Else
      ' For Replacing Out
          If lineArray(2) = "" Then
            WScript.Echo "Adding 'out' time " & currentTime & "."
          Else
            WScript.Echo "Replacing 'out' time " & lineArray (2) & " with " & currentTime & "."
          End If  
          editFile.WriteLine lineArray(0) & "," & _
                             lineArray(1) & "," & _
                             currentTime
        End If
      Else
      ' If The Input Date Not Found, Just Copy The
      ' Line To The New File Without Changing It
        editFile.WriteLine lineFull
	    End If
	  Loop
	  editFile.Close
    readFile.Close

    ' Replace Log With Log-Temp
      fso.DeleteFile "C:\CMD\hours\log\log.csv", True
      fso.MoveFile   "C:\CMD\hours\log\log-temp.csv", "C:\CMD\hours\log\log.csv"
	End Function

' Call if Row not found by Find-Row; makes new row
  Function NewRow(ByVal inOut, ByVal currentTime, ByVal searchDate)
  ' 8 == Append File
    Set fso  = CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\CMD\hours\log\log.csv",8,True)

  ' Write YYYY/mm/dd,hh:mm,hh:mm
    fso.WriteLine searchDate & "," & currentTime & ","
    fso.close
    Set fso = Nothing
    WScript.Echo "New entry added, " & searchDate & " at " & currentTime
  End Function

' This function finds whether the row is there
' or not... I think this could be combined with 
' the FindRow function, might improve this at some point
  Function FindRow(ByVal inOut, ByVal currentTime, ByVal searchDate)
    found = False

    Set fso  = CreateObject("Scripting.FileSystemObject")
	  Set logFile = fso.OpenTextFile("C:\CMD\hours\log\log.csv", 1)

  ' Get each line of file
	  Do While logFile.AtEndOfStream <> True

    ' Split line into array, comma delim
	    lineArray = Split(logFile.ReadLine, ",")
    ' Current date is the first of the array
	    lineDate  = DateValue(lineArray(0))

		' SEARCH --------------------------------------------------------
	    If lineDate = searchDate Then
        found = True
      ' IN -------------------------------------------------
        If inOut = "in" Then
          ' Check if 'In' is populated
          If lineArray(1) = "" Then
             Call AddTime(inOut,currentTime,searchDate)
						 Exit Do
          Else
            ' Check if overWrite is requested
              If OverWriteConfirmation(inOut) = "y" Then
                  Call AddTime(inOut,currentTime,searchDate)
									Exit Do
              Else
									Exit Do
              End If
          End If
      ' OUT ------------------------------------------------
        Else
          If lineArray(1) = "" Then
          ' Error, trying to clock out when there is no clock in data
            WScript.Echo "You cannot clock out when you haven't yet clocked in for the day."
            WScript.Echo "Please enter a clock in time."
						Exit Do
          ElseIf lineArray(2) = "" Then
            Call AddTime(inOut,currentTime,searchDate)
						Exit Do
          Else
            ' Check if overwrite is requested
              If OverWriteConfirmation(inOut) = "y" Then
                  Call AddTime(inOut,currentTime,searchDate)
									Exit Do
              Else
                  WScript.Echo "Exiting..."
									Exit Do
              End If
          End If
        End If
	    End If
	  Loop

	  logFile.Close
    Set logFile = Nothing

    ' Row wasn't found
    If found = False And inOut = "in" Then
      WScript.Echo "Attempting to record a new day..."
      Call NewRow(inOut,currentTime,searchDate)
    Else
      WScript.Echo "Error, not clocked in for this date. You need to clock in before you clock out!"
    End If

	End Function

  Function OverWriteConfirmation(ByVal inOut)
    If inOut = "in" Then
      WScript.Echo "You have already clocked in today."
    Else
      WScript.Echo "You have already clocked out today."
    End If
    WScript.StdOut.Write "Would you like to overwrite your current time? (y/n) : "
  ' Return confirmation
    OverWriteConfirmation = WScript.StdIn.ReadLine
  End Function

' Handle arguments from cl.bat
      Set args = WScript.Arguments
  ' In/Out = args(0)
      If args(0) = "in" Then
        inOut = "in"
      Else 
        inOut = "out"
      End If
  ' Time = args(1)
      If Not args(1) = "empty" Then
        currentTime = args(1)
      Else 
        currentTime = FormatDateTime(Now,4)
      End If
  ' Date = args(2)
      If Not args(2) = "empty" Then
        searchDate = DateValue(args(2))
      Else 
        searchDate = Date
      End If

    Call FindRow(inOut,currentTime,searchDate)
		WScript.Echo "huh?"
		Set pog = WScript.CreateObject("WScript.Shell")
		pog.Run "cl.vbs quiet"
		Set pog = Nothing
		WScript.Echo "Lol?"


