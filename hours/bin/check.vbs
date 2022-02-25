' Check.vbs    ~~ Julian Orchard [hello@julianorchard.co.uk]

'   Read log file and show how many
'   hours have been logged this current week

' Checks the log file to get the basic values to be
' used in all other outputs
  Function Initial
    Set logFile = CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\CMD\hours\log\log.csv", 1)
    ' Start of this week: https://stackoverflow.com/questions/8884098/find-this-weeks-monday
        startOfThisWeek = DateAdd("d", -((Weekday(Date) + 7 - 2) Mod 7), Date)
        daysOfTheWeekCount = 0

			' Get each line of file
				Do While logFile.AtEndOfStream <> True

				' Split line into array, comma delim
						lineArray = Split(logFile.ReadLine, ",")
' Current date is the first of the array
						lineDate = DateValue(lineArray(0))
						If lineDate = startOfThisWeek Then
								daysOfTheWeekCount = 5
						End If

						If daysOfTheWeekCount <> 0 Then
							startTime = lineArray(1)

						' Check an end time exists, do not try and count day if not
							If lineArray(2) = "" Then
								WScript.Echo "Warning, you have not clocked out for " & lineDate
							Else 
								endTime = lineArray(2)
								minutesWorked = DateDiff("n",TimeValue(startTime),TimeValue(endTime))
								minutesTotal = minutesTotal + minutesWorked
							End If
							daysOfTheWeekCount = daysOfTheWeekCount - 1
						End If
				Loop
			' Return Initial (Raw)
				Initial = minutesTotal
	' Close Log File
    logFile.Close
    Set logFile = Nothing
	End Function

	Function Simple(ByRef raw)
	' Progress Bar and Nice Output
			hoursTotal = raw/60
			barProgress = Round(hoursTotal)
			barFill = 40 - barProgress

		' Add spaces before the number to make it prettier
		' hoursTotal        of         39.5 hours, complete!
			If Len(Round(hoursTotal,1)) = 1 Then
				WScript.StdOut.Write vbNewLine & "   "
			ElseIf Len(Round(hoursTotal,1)) = 2 Then
				WScript.StdOut.Write vbNewLine & "  "
			ElseIf Len(Round(hoursTotal,1)) = 3 Then
				WScript.StdOut.Write vbNewLine & " "
			Else
				WScript.StdOut.Write vbNewLine & ""
			End If
			WScript.StdOut.Write vbNewLine & Round(hoursTotal,1) & "        of       39.5 hours, complete!"

		' Progress Bar Generation!
		' [###################-------]
			WScript.StdOut.Write vbNewLine & "["
		' Pound symbol content
				For i = 1 To barProgress
					WScript.StdOut.Write "#"
				Next 
		' Dash symbol content
				For j = 1 To barFill
					WScript.StdOut.Write "-"
				Next
			WScript.StdOut.Write "]" & vbNewLine
			Simple = hoursTotal
  End Function

	Function UpdateCMDRC(raw)
		hoursTotal = raw/60
	' Open
		Set cmdrcFile = CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\cmd\hours\log\cmdrc.txt", 2, true)
	' Output
		cmdrcFile.WriteLine(Round(hoursTotal,1))
	' Close
		cmdrcFile.Close
		Set cmdrcFile = Nothing
	End Function

' Verbose feature not implemented yet
  Function Verbose
      WScript.Echo "The Verbose output feature is not yet working. Please try again later."
  End Function

Set args = WScript.Arguments
raw = Initial ' This is the total minutes worked
If args(0) = "normal" Then
	Call Simple(raw)
	Call UpdateCMDRC(raw)
ElseIf args(0) = "quiet" Then
' To be called by clock.vbs
' as a default, eventually
	Call UpdateCMDRC(raw)
Else
' Not doing anything, at the moment...
	Call UpdateCMDRC(raw)
End If

