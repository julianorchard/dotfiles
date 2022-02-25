Sub Mail()

    Dim _
		msgTo, _
        msgSub
    As String
    msgTo  = "EMAIL"   ' The address to send to
    msgSub = "SUBJECT" ' The subject line
' Send the message
	With oMail
		.To = msgTo
		.Body = vbNewLine & vbNewLine & _
				"===================" & vbNewLine & vbNewLine & _
				"  Mail from Excel  " & vbNewLine & vbNewLine & _
				"===================" & vbNewLine & vbNewLine & _
				vbNewLine & vbNewLine & _
				"	MESSAGE CONTENT GOES HERE   " & vbNewLine & vbNewLine & _
				vbNewLine & vbNewLine & vbNewLine & vbNewLine & _
				"Note: This email was sent automatically, and not by me. " & vbNewLine & _
				"      Please do not respond, but let me know if sent in error."
		.Subject = msgSub
	' If you want attachments
		' .Attachments.Add "ATTACHMENT LOCATION"
		.Send
	End With
	' Generally a good 
	' idea to unset these
		Set oLook = Nothing
		Set oMail = Nothing

End Sub