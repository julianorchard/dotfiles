# File:       psrc.ps1
# Author:     Julian Orchard [hello@julianorchard.co.uk]
# Tag Added:  2022-02-24
# Desciption: Powershell Config File

# PS Prompt
  function prompt 
  {
    $dateTime = get-date -Format "HH:mm:ss"
    $currentDir = $(Get-Location)
    " $ Time: $dateTime
    $currentDir PowerShell > "
  }

# Set Wallpaper Function, From
# https://techexpert.tips/powershell/powershell-configure-wallpaper/
  function Set-Wallpaper($MyWallpaper)
  {
  $code = @' 
  using System.Runtime.InteropServices; 
  namespace Win32{ 
      
      public class Wallpaper{ 
          [DllImport("user32.dll", CharSet=CharSet.Auto)] 
          static extern int SystemParametersInfo (int uAction , int uParam , string lpvParam , int fuWinIni) ; 
          
          public static void SetWallpaper(string thePath){ 
              SystemParametersInfo(20,0,thePath,3); 
          }
      }
  } 
'@

  add-type $code 
  [Win32.Wallpaper]::SetWallpaper($MyWallpaper)
  }

# Return Readable Numbers from 0-99
  function Readable-Numbers($InputNumber,$NumberFormat) 
  {
  # Single Digits
    function One-Digit($in) 
    {
      switch ( $in.Substring($in.Length - 1) )
      {
        0 { 
          if ($in.Length -eq 1) 
          {
            $ReadableNumber = "Zero" 
          }
          else
          {
            $ReadableNumber = ""
          }
        }
        1 { $ReadableNumber = "One" }
        2 { $ReadableNumber = "Two" }
        3 { $ReadableNumber = "Three" }
        4 { $ReadableNumber = "Four" }
        5 { $ReadableNumber = "Five" }
        6 { $ReadableNumber = "Six" }
        7 { $ReadableNumber = "Seven" }
        8 { $ReadableNumber = "Eight" }
        9 { $ReadableNumber = "Nine" }
      }
      return $ReadableNumber
    }
  # Two Digit Numbers
    function Two-Digit($in) 
    {
      switch -Wildcard ( $in )
      {
        10 { $ReadableNumber = "Ten" }
        11 { $ReadableNumber = "Eleven" }
        12 { $ReadableNumber = "Twelve" }
        13 { $ReadableNumber = "Thirteen" }
        14 { $ReadableNumber = "Fourteen" }
        15 { $ReadableNumber = "Fifteen" }
        16 { $ReadableNumber = "Sixteen" }
        17 { $ReadableNumber = "Seventeen" }
        18 { $ReadableNumber = "Eighteen" }
        19 { $ReadableNumber = "Nineteen" }
        "[2-9][0-9]" 
        { 
          $i = $_.ToCharArray()
          $i1 = $i[0]
          $i2 = $i[1]
          switch ( $i1 ) 
          {
            2 { $ReadableNumber = "Twenty" }
            3 { $ReadableNumber = "Thirty" }
            4 { $ReadableNumber = "Forty" }
            5 { $ReadableNumber = "Fifty" }
            6 { $ReadableNumber = "Sixty" }
            7 { $ReadableNumber = "Seventy" }
            8 { $ReadableNumber = "Eighty" }
            9 { $ReadableNumber = "Ninety" }
         }
        # Add Dash or Not
          if ("$i2" -ne "0")
          {
            $ReadableNumber += "-"
          }
        # Get Unit Number
          $ReadableNumber += One-Digit($_)
        }
      }
      return $ReadableNumber
    }
    $in = $InputNumber.ToString()
    switch ( $in.Length )
    {
      1 {
      # Formatting 0-9
        One-Digit($in)
      }
      2 {
      # Formatting 10-99
        Two-Digit($in)
      }
      default {
      # This is all it can handle
        return ""
      }
    }
  }

# PS Aliases
  function dev { cd C:\CMD }
  function home { cd ~ }
  function ll { ls }
  function uk { cd "~\Documents\Website\2) UK Site\" }
  function us { cd "~\Documents\Website\3) Export\US Site" }
  function ca { cd "~\Documents\Website\3) Export\CA Site" }
  function eu { cd "~\Documents\Website\3) Export\EU Site" }

 
