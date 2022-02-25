' On Range or Cell Click
Private Sub Worksheet_SelectionChange(ByVal Target As Range)
    If Selection.count = 1 Then
	'   Range Goes Below
        If Not Intersect(Target, Range("RANGE")) Is Nothing Then
		'   You can define a wider range in the _
		    above part, and you can act differently _
			depending on a range within that range 
			
			If Not Intersect(Target, Range("SUBRANGE")) Is Nothing Then
				' Do a thing
			Else
				' Do something else
			End If
        End If
    End If
End Sub