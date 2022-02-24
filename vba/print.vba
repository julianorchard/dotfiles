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
        ' PREPRINT
            FileName = GetFileName
        Else
        ' PREPRINT
            FileName = AutoFileName
        End If
        
        
        Call PrePrint(ByVal FileName)
        
      ' Save Location
        MsgBox ("Document Save Successful: " & DesktopPath & "/" & FileName)
    
End Sub


'   PRE-PRINT


Private Sub PrePrint(ByVal FileName As Variant)
    If IsFile(FileName) = True Then
        'file already exists
           
         Ret = IsFileOpen(fNameString(ByVal fNameItems))
         If Ret = True Then
          ' file already open
            If MsgBox("The file is already open, do you want " _
            & "to save it as a different file name?", vbYesNo _
            + vbQuestion, "Save as New?") = vbYes Then
                Call NewFileSave(ByVal fNameItems)
            Else
                ' Action Cancelled
                MsgBox "Action Cancelled", vbCritical, "Cancelled"
                End
            End If
         Else
          ' file isn't open, custom decision
            decis = MsgBox("A file already exists with this name. " _
            & "Would you like to overwrite the current file (this will " _
            & "erase the current file). If so, select 'yes'." & vbNewLine & vbNewLine _
            & "If you would like to save it as a new file, select 'no'.", 3, _
            "Overwrite or Save As New File?")
            If decis = 6 Then
               Call PrintSub(ByVal fNameString(ByVal fNameItems))
            ElseIf decis = 7 Then
               Call NewFileSave(ByVal fNameItems)
            Else
               End
            End If
         End If
    Else
        ' file not found
          Call PrintSub(ByVal fNameString(ByVal fNameItems))
    End If
End Sub


'   PRINT


Private Sub PrintSub(ByVal FileName As String)
    ' print
      ActiveSheet.ExportAsFixedFormat _
      Type:=xlTypePDF, _
      FileName:=FileName, _
      Quality:=xlQualityStandard, _
      IncludeDocProperties:=True, _
      IgnorePrintAreas:=False, _
      OpenAfterPublish:=True
    ' message
      MsgBox "File saved as: " & vbNewLine _
      & vbNewLine & fNameString, vbInformation, "Save Sucessful"
End Sub


'   NEW FILE SAVE


Private Sub NewFileSave(ByVal fNameItems As Variant)
    Dim counter As Integer
    fNameItems(6) = " - "
    Do
        counter = counter + 1
        fNameItems(7) = counter
    Loop While IsFile(fNameString(ByVal fNameItems)) = True
    Call PrintSub(ByVal fNameString(ByVal fNameItems))
End Sub


'   DOES FILE EXIST


Function IsFile(ByVal fNameString As String) As Boolean
    ' does file exist function, thanks stack overflow
      On Error Resume Next
      IsFile = ((GetAttr(fNameString) And vbDirectory) <> vbDirectory)
End Function


'   IS FILE OPEN


Function IsFileOpen(FileName As String)
    ' is file open function, from stackoverflow
      Dim ff As Long, ErrNo As Long
      On Error Resume Next
      ff = FreeFile()
      Open FileName For Input Lock Read As #ff
      Close ff
      ErrNo = Err
      On Error GoTo 0
      Select Case ErrNo
      Case 0:    IsFileOpen = False
      Case 70:   IsFileOpen = True
      Case Else: Error ErrNo
      End Select
End Function


'   ARRAY TO STRING


Function fNameString(ByVal fNameItems As Variant) As String
    Dim fName As String
    fNameString = Join(fNameItems, "")
End Function


