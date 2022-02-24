' https://stackoverflow.com/questions/8807153/vbscript-to-convert-word-doc-to-pdf
' Thank you very much this is very useful

Function DocToPdf(docInputFile, pdfOutputFile, fileSystemObject)
'  Dim fileSystemObject
  Dim wordApplication
  Dim wordDocument
  Dim wordDocuments
  Dim baseFolder
'  Set fileSystemObject = CreateObject("Scripting.FileSystemObject")
  Set wordApplication = CreateObject("Word.Application")
  Set wordDocuments = wordApplication.Documents

  docInputFile = fileSystemObject.GetAbsolutePathName(docInputFile)
  baseFolder = fileSystemObject.GetParentFolderName(docInputFile)

' Parent Folder
  If Len(fileSystemObject.GetParentFolderName(pdfOutputFile)) = 0 Then
    pdfOutputFile = baseFolder + "\" + pdfOutputFile
  End If

' Disable any potential macros of the word document.
  wordApplication.WordBasic.DisableAutoMacros
  Set wordDocument = wordDocuments.Open(docInputFile)

' More info:
' http://msdn2.microsoft.com/en-us/library/bb221597.aspx
  wordDocument.SaveAs pdfOutputFile, wdFormatPDF
  wordDocument.Close WdDoNotSaveChanges
  wordApplication.Quit WdDoNotSaveChanges
' Unset
  Set wordApplication = Nothing
  Set fileSystemObject = Nothing

End Function

' Call this with
' the dtp.bat file
Set args = wscript.arguments 
docInputFile = args(0)
Set fileSystemObject = CreateObject("Scripting.FileSystemObject")
If wscript.arguments.count = 1 Then
' Automatically call file filename+.pdf
' If Len(pdfOutputFile) = 0 Then
'  Set fso = CreateObject("Scripting.FileSystemObject")
  docInputFile  = fileSystemObject.GetAbsolutePathName(docInputFile)
  pdfOutputFile = fileSystemObject.GetBaseName(docInputFile) + ".pdf"
Else 
  pdfOutputFile = args(1)
End If 

Call DocToPdf(docInputFile, pdfOutputFile, fileSystemObject)