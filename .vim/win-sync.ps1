# File:       win-sync.ps1
# Author:     Julian Orchard <hello@julianorchard.co.uk>
# Tag Added:  2022-03-29
# Desciption: Does some stuff for vim on Windows.

# Get Plug Install
  If (-Not (Test-Path -Path "$HOME\.vim\autoload\plug.vim"))
  {
    $PlugURL="https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    Invoke-WebRequest -UseBasicParsing $PlugURL | New-Item $HOME\.vim\autoload\plug.vim -Force
  }
# Copy *imrc (vimrc and gvimrc) to Win Location
  Copy-Item "$HOME\.vim\*imrc" -Destination "$HOME\vimfiles\."
