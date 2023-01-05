##   psrc.ps1  ---  PowerShell Config File

# Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

## Description:

# A small powershell configuration file with a few custom functions.

## License:

# See /LICENSE file in the root of this repository.

## Code:


Set-ExecutionPolicy Bypass

function prompt {
    " $ $(Split-Path -Path (Get-Location) -Leaf) > "
}

function Set-Wallpaper($MyWallpaper) {
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

function Get-Shortcut {
  param(
    $path = $null
  )
  $obj = New-Object -ComObject WScript.Shell
  if ($path -eq $null) {
    $pathUser = [System.Environment]::GetFolderPath('StartMenu')
    $pathCommon = $obj.SpecialFolders.Item('AllUsersStartMenu')
    $path = dir $pathUser, $pathCommon -Filter *.lnk -Recurse
  }
  if ($path -is [string]) {
    $path = dir $path -Filter *.lnk
  }
  $path | ForEach-Object {
    if ($_ -is [string]) {
      $_ = dir $_ -Filter *.lnk
    }
    if ($_) {
      $link = $obj.CreateShortcut($_.FullName)

      $info = @{}
      $info.Hotkey = $link.Hotkey
      $info.TargetPath = $link.TargetPath
      $info.LinkPath = $link.FullName
      $info.Arguments = $link.Arguments
      $info.Target = try {Split-Path $info.TargetPath -Leaf } catch { 'n/a'}
      $info.Link = try { Split-Path $info.LinkPath -Leaf } catch { 'n/a'}
      $info.WindowStyle = $link.WindowStyle
      $info.IconLocation = $link.IconLocation

      New-Object PSObject -Property $info
    }
  }
}
function Set-Shortcut {
  param(
  [Parameter(ValueFromPipelineByPropertyName=$true)]
  $LinkPath,
  $Hotkey,
  $IconLocation,
  $Arguments,
  $TargetPath
  )
  begin {
    $shell = New-Object -ComObject WScript.Shell
  }
  process {
    $link = $shell.CreateShortcut($LinkPath)

    $PSCmdlet.MyInvocation.BoundParameters.GetEnumerator() |
      Where-Object { $_.key -ne 'LinkPath' } |
      ForEach-Object { $link.$($_.key) = $_.value }
    $link.Save()
  }
}

function Readable-Numbers($InputNumber,$NumberFormat)
{
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
                if ("$i2" -ne "0")
                {
                    $ReadableNumber += "-"
                }
                $ReadableNumber += One-Digit($_)
            }
        }
        return $ReadableNumber
    }
    $in = $InputNumber.ToString()
    switch ( $in.Length )
    {
        1 {
            One-Digit($in)
        }
        2 {
            Two-Digit($in)
        }
        default {
            return ""
        }
    }
}

function dev { cd C:\CMD }
function home { cd ~ }
function ll { ls }
function uk { cd "~\Documents\Website\2) UK Site\" }
function us { cd "~\Documents\Website\3) Export\US Site" }
function ca { cd "~\Documents\Website\3) Export\CA Site" }
function eu { cd "~\Documents\Website\3) Export\EU Site" }

cd $env:userprofile
