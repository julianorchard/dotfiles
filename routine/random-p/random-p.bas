' Run By C:\CMD\routine\random-p\random-p.ahk
' Finds Current Day, Sends Email Attaching Image

' Initialise using Workbook_Open Function in ThisWorkbook
' 		Private Sub Workbook_Open()
'			Main
'		End Sub

    Function Mail(ByVal image As String)
        Dim messageSubject, sendList As String
        Dim oLook, oMail As Object

        Set oLook = CreateObject("Outlook.Application")
        Set oMail = oLook.CreateItem(0)
        
    
        messageSubject = "P:\ Of The Week - AUTOMATED"
        sendList = "EMAIL_HERE;EMAIL_HERE;"	' 					< = ==     =  =  Add 'To' Emails Here
        

        With oMail
            .To = sendList
            .Body = "Hello! " & vbNewLine & vbNewLine & _
                    "Here is your random P:\ image of the week (attached). " & vbNewLine & vbNewLine & _
                    "----------" & vbNewLine & vbNewLine & _
                    "This email was sent automatically from J:\TSD\Xlsm\random-p.xlsm."
            .Subject = messageSubject
            .Attachments.Add image
            .Send
        End With
        Set oLook = Nothing
        Set oMail = Nothing
    End Function

    Sub Main()
        Sheet1.Activate
        Dim dater As Range      ' The GOod
        Dim c As Integer: c = 0 ' The  bAD
        Dim image As String     ' The .jpg
        
    ' Change this cell depending
    ' on the sheet layout (example
    ' .csv in \routine\random-p\random-p.csv):
        Set dater = Sheet1.Range("A2")
        
            Do
                Set dater = dater.Offset(1, 0)
                    c = c + 1
                    dater.Offset(0, -1).Select
            Loop Until dater = Date Or c >= 1000 ' * NOTE:
            ' 1,000 lines is more than enough for this program
        
        ' Get image path as string from cell to
        ' the left of today's date:
            image = dater.Offset(0, -1).Value
            
            If image <> "" Then
                Mail (image)
            End If
            
    ' Automatically exit the file
    ' after the macro gets run:
        'Application.Quit
	' Disable Macros in Trust Centre Settings
	' if you need to open the file...
        
    End Sub
