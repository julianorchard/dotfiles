' File:       pape-set.vbs
' Author:     Julian Orchard <hello@julianorchard.co.uk>
' Tag Added:  2022-02-17
' Desciption: Add current date and time text to your background... horrible, Windows 10

	username = CreateObject("WScript.Network").UserName
	datetime = FormatDateTime(now, 4)

' -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
' String 1: e.g. "It's Quater Past Twelve"
	allTimes=Split("One Two Three Four Five Six Seven Eight Nine Ten Eleven Twelve One")
	hours   = Split(FormatDateTime(Now, 4),":")(0)
	minutes = Split(FormatDateTime(Now, 4),":")(1)
	morning = "morning"
	If CInt(hours) > 10 Then
		LTrim(CStr(hours))
	End If
	If CInt(hours) => 12 Then
		morning = "afternoon"
		If Not CInt(hours) = 12 Then
			hours = CInt(hours) - 12
		End If
	End If
	hourtext = allTimes(CInt(hours)-1)
	hournext = allTimes(CInt(hours))
	If CInt(minutes) =< 5 Then
		str1 = "It's " & hourtext & " O'Clock"
	ElseIf CInt(minutes) > 5 And CInt(minutes) =< 11 Then
		str1 = "It's Ten Past " & hourtext 
	ElseIf CInt(minutes) > 11 And CInt(minutes) =< 18 Then
		str1 = "It's Quater Past " & hourtext
	ElseIf CInt(minutes) > 18 And CInt(minutes) =< 25 Then
		str1 = "It's Twenty Past " & hourtext
	ElseIf CInt(minutes) > 25 And CInt(minutes) =< 35 Then
		str1 = "It's Half Past " & hourtext
	ElseIf CInt(minutes) > 35 And CInt(minutes) =< 41 Then 
		str1 = "It's Twenty To " & hournext
	ElseIf CInt(minutes) > 41 And CInt(minutes) =< 48 Then
		str1 = "It's Quater To " & hournext
	ElseIf CInt(minutes) > 48 And CInt(minutes) =< 55 Then
		str1 = "It's Ten To " & hournext
	ElseIf CInt(minutes) > 55 Then
		str1 = "It's " & hournext & " O'Clock"
	Else
		str1 = "It's certainly a time of day"
	End If

' -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
' String 2: e.g. "in the afternoon, on Monday the 21st of March, 2022"
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
		'Found
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
	str2 = "in the " & morning & _
	", on " & nameofweekday & _
	" the " & normalmonth & dateending & _
	" of " & Split(FormatDateTime(Now,1))(1) & _
	", " & Split(FormatDateTime(Now,1))(2)

' -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
' Get Colour From bin/colours.txt File
  With CreateObject("Scripting.FileSystemObject")
    hexcolour = .OpenTextFile("C:\cmd\wallpapers\bin\colours.txt").ReadAll()
    savedcolour = .OpenTextFile("C:\cmd\wallpapers\bin\colours.txt").ReadAll()
    .CopyFile "C:\cmd\wallpapers\bin\colours.txt", "C:\cmd\wallpapers\bin\colours-saved.txt"
	End With 
  ' Default White Text, If Error 
  If hexcolour = Empty Then 
    hexcolour = "#FFFFFF" 
  End If

' -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
' Save some system resources...
' Curtext
  With CreateObject("Scripting.FileSystemObject")
    filestr = .OpenTextFile("C:\cmd\wallpapers\bin\cur.txt").ReadAll()
	End With 
' CurStr
  curstr = str1 & " " & str2

  If filestr <> curstr _
  Or hexcolour <> savedcolour Then
    Set newtext = CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\cmd\wallpapers\bin\cur.txt",2)
    newtext.WriteLine(curstr) 
' -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
' Set the Wallpaper with CMD's
'   With CreateObject("WScript.Shell")
'     .CurrentDirectory = "C:\cmd\wallpapers\"
'     .Run "magick convert C:\cmd\wallpapers\current.jpg -pointsize 80 -stroke #222 -strokewidth 1.5 -fill " & _
'             hexcolour & " -gravity Center -font Times-New-Roman-Bold -annotate +0-260 """ & _
'             str1 & """ -pointsize 50 -font Gill-Sans-Condensed -strokewidth 1 -annotate +0-180 """ & str2 & _
'             """ C:\cmd\wallpapers\current.bmp", 0, true
'     WScript.Sleep 10000
'     .Run "reg add ""HKEY_CURRENT_USER\Control Panel\Desktop"" /v Wallpaper /t REG_SZ /d C:\cmd\wallpapers\current.bmp /f", 0, true
'     .Run "RUNDLL32.EXE user32.dll,UpdatePerUserSystemParameters", 0, true
'   End With
   Set shell = CreateObject("WScript.Shell")
   shell.CurrentDirectory = "C:\cmd\wallpapers\"
 '	Convert Imagemagick 
   shell.Run "magick convert C:\cmd\wallpapers\current.jpg -pointsize 80 -stroke #222 -strokewidth 1.5 -fill " & _
             hexcolour & " -gravity Center -font Times-New-Roman-Bold -annotate +0-260 """ & _
             str1 & """ -pointsize 50 -font Gill-Sans-Condensed -strokewidth 1 -annotate +0-180 """ & str2 & _
             """ C:\cmd\wallpapers\current.bmp", 0, true
 ' Wait For The Magick To Happen	
    WScript.Sleep 10000
 ' Set Paper (pain...)
    shell.Run "reg add ""HKEY_CURRENT_USER\Control Panel\Desktop"" /v Wallpaper /t REG_SZ /d C:\cmd\wallpapers\current.bmp /f", 0, true
    shell.Run "RUNDLL32.EXE user32.dll,UpdatePerUserSystemParameters", 0, true
    newtext.Close
    Set newtext = Nothing
  End If
