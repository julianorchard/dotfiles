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
      $hourInt = Get-Date -Format "h."
    
    # Remove Trailing (.) in Hour
      $hourInt = $hourInt.Substring(0, $hourInt.Length - 1)
    # Fixing Zero-Leading Minute
      $m = $minsInt.ToCharArray()
      $m1 = $m[0]
      $m2 = $m[1]
      if ( $m1 -eq "0" ) { $minsInt = $m2.ToString() }

    # Past/To The Hour/Next
      if ([int]$minsInt -lt 35) 
      {
        $toOrPast = "Past"
      }
      else 
      {
        $toOrPast = "To"
        $minsInt = 60 - $minsInt
        $hourInt = [int]$hourInt + 1
      }

    # Make It Readable
      $currentMinutes = Readable-Numbers($minsInt)
      $currentHour = Readable-Numbers($hourInt)
      switch ( $currentMinutes )
      {
        "One" { $minType = " Minute" }
        default { $minType = " Minutes" }
      }
      switch ( $currentMinutes ) 
      {
        "Fifteen" { 
          $currentMinutes = "Quarter" 
          $minType = ""
        }
        "Thirty" { 
          $currentMinutes = "Half" 
          $minType = ""
        }
      }
      if ( $currentHour -eq "Thirteen" )
      {
        $currentHour = "One"
      }
      if ( $currentMinutes -eq "Zero" ) 
      {
        return "It's $currentHour O'Clock"
      } 
      else 
      {
        return "It's $currentMinutes$minType $toOrPast $currentHour"
      }
  }

# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~- STR 2 FUNCTION -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  function String-Line-Two
  {
  # Morning/Afternoon/Evening
    if ((Get-Date -Format "tt") -eq "AM") 
    {
      $amOrPm = "morning"
    }
    else 
    {
      $amOrPm = "afternoon"
    }
  # Day (getting 'th/nd/etc')
    $dayAndMonth = (Get-Date -Format "M").ToString() -Split " "
    $day = $dayAndMonth[0]
    $dayEnd = switch -Regex ($day) 
    {
      {@("3", "23") -contains $_} { "rd" }
      {@("2", "22") -contains $_} { "nd" }
      {@("1", "21", "31") -contains $_} { "st" }
      default { "th" }
    }
  # Month
    $month = $dayAndMonth[1]
  # Day of the Week
    $dayOfTheWeek = Get-Date -Format "dddd"
  # Year 
    $year = Get-Date -Format "yyyy"

    return "in the $amOrPm on $dayOfTheWeek the $day$dayEnd of $month, $year"
  }


while ($true)
{
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~- MAIN -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# Wallpaper Location (Could Be Randomized Eventually...)
  $paperIn = "C:\cmd\wallpapers\current.jpg"
  $paperOut = "C:\cmd\wallpapers\current.bmp"

# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
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
    $strLineTwo = String-Line-Two
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
    $arguments = 'convert',"$paperIn",'-pointsize','60','-stroke','#222','-strokewidth','1.5','-fill',$currentColor,'-gravity','Center','-font','Times-New-Roman-Bold','-annotate','+0-260',"$strLineOne",'-pointsize','30','-font','Times-New-Roman','-strokewidth','0.8','-annotate','+0-200',"$strLineTwo",$paperOut
    & magick $arguments
  
# Set the Wallpaper
  Set-Wallpaper($paperOut)
  Start-Sleep -Seconds 15
}
