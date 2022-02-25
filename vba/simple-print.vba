' NOTE:
'   Make the Button: Print Object = False


' Get File Name Function:
'       Manually input the FileName
Public Function GetFileName() As String
    GetFileName = _
    InputBox("What do you want to call the PDF?")
    If GetFileName = vbNullString Then
        MsgBox ("Print to PDF Cancelled.")
        End
    Else
        Exit Function
    End If
End Function

Private Sub PrintPDF_Click()

' Desktop Path
        Dim DesktopPath As String
        DesktopPath = CreateObject("WScript.Shell").specialfolders("Desktop")
    
' File Name Automation

    ' Specify the name of the file as a
    ' cell value in the sheet
    
        Dim AutoName As Range
        Set AutoName = Sheet1.Range("XD100")   ' Example Range
        Dim AutoFileName As String
            AutoFileName = AutoName.Value
    
        Dim FileName As String
        
        If AutoFileName = "" Then
            
        ' Manually Input FileName
            FileName = GetFileName
    
        Else
        
        ' Auto Input FileName
            FileName = AutoFileName
            
        End If
    
    ' PRINT
    
        ActiveSheet.ExportAsFixedFormat _
        Type:=xlTypePDF, _
        FileName:=DesktopPath & "\" & _
        FileName & ".pdf", _
        Quality:=xlQualityStandard, _
        IncludeDocProperties:=True, _
        IgnorePrintAreas:=False, _
        OpenAfterPublish:=False            ' Open After Publishing True/False
        
      ' Save Location
        MsgBox ("Document Save Successful: " & DesktopPath & "/" & FileName)
    
End Sub
