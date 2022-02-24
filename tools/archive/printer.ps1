# Archived in favour of a .vbs solution...



# Forced to use a Powershell file to print
# simply because the printer name has spaces... 

# 	   Get-Content -Path  | Out-Printer -Name   
#        	---- doesn't work for what I need

Set-ExecutionPolicy -ExecutionPolicy Bypass
# Unless testing, called from C:\CMD\ahk\printer.ahk script
	foreach($line in Get-Content C:\CMD\ahk\res\printlist.txt) {
	  if ($line -match $regex) {
      Start-Process –FilePath 'bruh' –Verb Print -PassThru
	  }
	}

# Clear the printlist.txt file
	#Clear-Content C:\CMD\ahk\res\printlist.txt
