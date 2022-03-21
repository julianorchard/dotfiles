' Save some system resources; 
' skip if same text as before (nothing, therefore, needs doing...)
	'read (1)
	Set curtext = CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\Users\jorchard\wallpapers\bin\cur.txt",1)
	'write (2)
	Set newtext = CreateObject("Scripting.FileSystemObject").OpenTextFile("C:\Users\jorchard\wallpapers\bin\cur.txt",2,True)
	currenttext = curtext.ReadAll()
	replacetext = str1 & " " & str2
	If currenttext = replacetext Then
		WScript.Echo "Text is the same"
	Else
		newtext.WriteLine(replacetext)
		WScript.Echo "Text is different"
	End If
	curtext.Close
	newtext.Close
	Set newtext = Nothing
	Set curtext = Nothing

