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

# PS Aliases
  function dev { cd C:\CMD }
  function home { cd ~ }
  function ll { ls }

 
