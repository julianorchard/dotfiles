##   psrc.ps1  ---  PowerShell Config File

# Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

## Description:

# A small powershell configuration file with a few custom functions.

## License:

# See /LICENSE file in the root of this repository.

## Code:

Set-ExecutionPolicy -Scope CurrentUser Bypass

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

function dev { cd C:\CMD }
function home { cd ~ }
function ll { ls }
function uk { cd "~\Documents\Website\2) UK Site\" }
function us { cd "~\Documents\Website\3) Export\US Site" }
function ca { cd "~\Documents\Website\3) Export\CA Site" }
function eu { cd "~\Documents\Website\3) Export\EU Site" }

cd $env:userprofile
