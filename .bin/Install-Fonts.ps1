##   Install-Fonts.ps1  ---  Install some fonts.

# Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

## Description:

# Install some of my favourite (and most relevant to this set of
# dotfiles) fonts:

# - [Anonymice NF](https://github.com/ryanoasis/nerd-fonts/)
# - [Fira Code](https://github.com/tonsky/FiraCode/)
# - [Fira Code Mono Italic](https://github.com/zwaldowski/Fira/raw/zwaldowski/mod-new/otf/)

## License:

# See /LICENSE file in the root of this repository.

## Code:

$fontArray = @(

    @("Anonymice Nerd Font Complete Windows Compatible.ttf","https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/AnonymousPro/complete/Anonymice%20Nerd%20Font%20Complete%20Windows%20Compatible.ttf"),
    @("FiraCode-VF.ttf","https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip"),
    @("FiraMono-MediumItalic.otf","https://github.com/zwaldowski/Fira/raw/zwaldowski/mod-new/otf/FiraMono-MediumItalic.otf")

)

ForEach ($font In $fontArray) {

    $fontName = $font[0]
    $fontURL  = $font[1]
    $fontDownloadLocation = "$env:TEMP\$fontName"

    Write-Host "Installing: $fontName"

    If ([IO.Path]::GetExtension($fontURL) -eq '.zip') {
        ## Extract a zip file first
        Try {
            $fontArchive="$env:TEMP\font-install-temp.zip"
            (New-Object System.Net.WebClient).DownloadFile($fontURL,$fontArchive)
        } Catch {
            Write-Host "! Error downloading $fontURL"
        }
        Expand-Archive $fontArchive -Force
        (New-Object -comObject Shell.Application).Namespace(0x14).CopyHere((Get-ChildItem -Path "$env:TEMP\font-install-temp" -Include $fontName -File -Recurse).FullName,0x10)
        Remove-Item "$env:TEMP\font-install-temp*" -Force -Recurse

    } Else {
        ## Just installing a font file
        Try {
            (New-Object System.Net.WebClient).DownloadFile($fontURL,$fontDownloadLocation)
        } Catch {
            Write-Host "! Error downloading $fontURL"
        }
        (New-Object -comObject Shell.Application).Namespace(0x14).CopyHere($fontDownloadLocation,0x10)
        Remove-Item $fontDownloadLocation -Force

    }

}
