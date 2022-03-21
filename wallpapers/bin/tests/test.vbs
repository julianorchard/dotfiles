' File:       test.vbs
' Author:     Julian Orchard [hello@julianorchard.co.uk]
' Tag Added:  2022-02-18
' Desciption: Test file generates the text to overlay onto image

' Part I
	allTimes=Split("One Two Three Four Five Six Seven Eight Nine Ten Eleven Twelve")
	hours   = Split(FormatDateTime(Now, 4),":")(0)
	minutes = Split(FormatDateTime(Now, 4),":")(1)
	morning = "morning"
	If CInt(hours) > 10 Then
		LTrim(CStr(hours))
	ElseIf CInt(hours) > 12 Then
		morning = "afternoon"
		hours = CInt(hours) - 12
	End If
	a=0
	For Each t In allTimes
		a=a+1
		If a = CInt(hours) Then
			hourtext = t
			hournext = allTimes(a+1)
		End If
	Next

	If CInt(minutes) < 5 Then
		WScript.Echo "It's " & hourtext & " O'Clock"
	ElseIf CInt(minutes) > 5 And CInt(minutes) =< 11 Then
		WScript.Echo "It's Ten Past " & hourtext 
	ElseIf CInt(minutes) > 11 And CInt(minutes) =< 18 Then
		WScript.Echo "It's Quater Past " & hourtext
	ElseIf CInt(minutes) > 18 And CInt(minutes) =< 25 Then
		WScript.Echo "It's Twenty Past " & hourtext
	ElseIf CInt(minutes) > 25 And CInt(minutes) =< 35 Then
		WScript.Echo "It's Half Past " & hourtext
	ElseIf CInt(minutes) > 35 And CInt(minutes) =< 41 Then 
		WScript.Echo "It's Twenty To " & hournext
	ElseIf CInt(minutes) > 41 And CInt(minutes) =< 48 Then
		WScript.Echo "It's Quater To " & hournext
	ElseIf CInt(minutes) > 48 And CInt(minutes) =< 55 Then
		WScript.Echo "It's Ten To " & hournext
	ElseIf CInt(minutes) > 55 Then
		WScript.Echo "It's " & hournext & " O'Clock"
	Else
		WScript.Echo "It's certainly a time of day"
	End If


' Part 2
	allDays=Split("Saturday Sunday Monday Tuesday Wednesday Thursday Friday")
	c=0
	For Each wdn In allDays
		If Weekday(Date) = c Then
			nameofweekday = wdn
		End If
		c=c+1
	Next
	dateending = ""
	normalmonth=Split(FormatDateTime(Now,1))(0)
	For i = 1 To 31
		If normalmonth = CStr(i) Then
			If CStr(i) = 3 Or CStr(i) = 23 Then
				dateending = "rd"
			ElseIf CStr(i) = 2 Or CStr(i) = 22 Then
				dateending = "nd"
			ElseIf CStr(i) = 1 Or CStr(i) = 21 Or CStr(i) = 31 Then
				dateending = "st"
			Else
				dateending = "th"
			End If
		End If
	Next
	WScript.Echo "in the " & morning & _
	", on " & nameofweekday & _
	" the " & normalmonth & dateending & _
	" of " & Split(FormatDateTime(Now,1))(1) & _
	", " & Split(FormatDateTime(Now,1))(2)
