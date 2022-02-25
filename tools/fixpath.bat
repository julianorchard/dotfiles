@echo off
 ::  File:       fixpath.bat
 ::  Author:     Julian Orchard [hello@julianorchard.co.uk]
 ::  Tag Added:  2022-02-24
 ::  Desciption: POV, adding/changing PATH but you're not admin...

	reg add HKEY_CURRENT_USER\Environment /v PATH /d "%PATH%;C:\cmd;C:\cmd\ahk;C:\cmd\ahk\res;C:\cmd\ahk\temp;C:\cmd\bin;C:\cmd\fun;C:\cmd\fun\archive;C:\cmd\hours;C:\cmd\hours\bin;C:\cmd\hours\log;C:\cmd\routine;C:\cmd\shortcuts;C:\cmd\tools;C:\cmd\tools\archive;C:\cmd\tools\layouts;C:\cmd\tools\nix;C:\cmd\tools\smol;C:\cmd\tools\wip;C:\cmd\vba;C:\cmd\vba\events;C:\cmd\vba\mail;C:\cmd\vba\print;C:\Users\%USERNAME%\Documents\Personal Admin\Vim\vim81;C:\Program Files\VideoLAN"

	call C:\cmd\bin\refresh.bat
