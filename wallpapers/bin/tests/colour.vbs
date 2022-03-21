' Getting Colour, Test
  With CreateObject("WScript.Shell")
    .Run "magick convert C:\cmd\wallpapers\current.jpg +dither -colors 1 -unique-colours txt: >C:\cmd\wallpapers\bin\colour.txt", 0, True
  End With
  WScript.Sleep 5000
  Dim colourOut
  With CreateObject("Scripting.FileSystemObject")
    colourOut = .OpenTextFile("C:\cmd\wallpapers\bin\colour.txt").ReadAll()
    .DeleteFile "C:\cmd\wallpapers\bin\colour.txt"
	End With 
	WScript.Echo "color = " & colourOut
  
