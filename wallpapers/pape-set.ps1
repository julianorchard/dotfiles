# File:       pape-set.ps1
# Author:     Julian Orchard <hello@julianorchard.co.uk>
# Tag Added:  2022-03-22
# Desciption: Rewriting my pape-set.vbs script in 
#             Powershell (it sets the wallpaper more consistently)

# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~- STR 1 FUNCTION -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  function String-Line-One() 
  {
    # Minutes To/Past
      $minsInt = Get-Date -Format "mm"
      if ( $minsInt -lt 35 ) 
      {
        $toOrPast = "Past"
      }
      else 
      {
        $toOrPast = "To"
        $currentMinutes = 60 - $minsInt
      }
      $currentMinutes = Readable-Numbers($minsInt)
    # 12 Hour Format
      $hourInt = Get-Date -Format "hh"
      $h = $hourInt.ToCharArray()
      $h1 = $h[0]
      $h2 = $h[1]
      if ( "$h1" -eq "0" ) 
      {
        $hourInt = $h2
      }
      $currentHour = Readable-Numbers($hourInt)
      return "It's $currentMinutes $toOrPast $currentHour"
  }

# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~- STR 2 FUNCTION -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  function String-Line-Two
  {
    return "in the (AM/PM) on the (dd)th of (mm), YYYY"
  }

# Wallpaper Location (Could Be Randomized Eventually...)
  $paperIn = "C:\cmd\wallpapers\current.jpg"
  $paperOut = "C:\cmd\wallpapers\current.bmp"



# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~- MAIN -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# CALL/CATCH STRING 1
  try
  {
    $strLineOne = String-Line-One
  }
  catch 
  {
    $strLineOne = "Have a good day"
  }
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# CALL/CATCH STRING 2
  try 
  {
    $strLineTwo = "this is a second, smaller test string."
  }
  catch 
  {
    $strLineTwo = "... and try to remember to fix this background at some point."
  }
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~- 
# COLOURS
  try 
  {
  # Get Current and Saved Colours
    $currentColor = Get-Content -Path "C:\cmd\wallpapers\bin\colours.txt" -TotalCount 1
    $savedColor   = Get-Content -Path "C:\cmd\wallpapers\bin\colours-saved.txt" -TotalCount 1
  # Copy Current Colour to Saved Colour
    Copy-Item "C:\cmd\wallpapers\bin\colours.txt" -Destination "C:\cmd\wallpapers\bin\colours-saved.txt"
  }
  catch 
  { 
    $currentColor = "#FFF" 
  }
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# ImageMagick Set Wallpaper
# if ($currentcolor -ne $savedcolor) # -and (check text) TESTING
  if ($currentcolor -eq $savedcolor) # -and (check text)
  {
    $arguments = 'convert',"$paperIn",'-pointsize','80','-stroke','#222','-strokewidth','1.5','-fill',$currentColor,'-gravity','Center','-font','Times-New-Roman-Bold','-annotate','+0-260',"$strLineOne",'-pointsize','50','-font','Gill-Sans-Condensed','-strokewidth','1','-annotate','+0-180',"$strLineTwo",$paperOut
    & magick $arguments
  } 
  else 
  {
    Write-Host "Not updating."
  }
# Set the Wallpaper
  Set-Wallpaper($paperOut)
