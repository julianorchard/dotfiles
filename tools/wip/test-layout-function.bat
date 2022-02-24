@echo off

:: https://www.dostips.com/forum/viewtopic.php?t=4027
:: Could be really useful for something in the future (immediately thought of 'hours' but 
:: idk if there's any point in using this for that given that it's coming from VBA anyway...

setlocal
set "format=Name: [25] Origin: [15] Age: [-3]"
call :Format "%format%" "Gerd Mueller" "Berlin" 45
call :Format "%format%" "Sally Whiteoak" "New York" 9
call :Format "%format%" "Richard Edgarson" "New Orleans" 103
exit /b

:Format Fmt [Str1] [Str2]...
setlocal disableDelayedExpansion
set "fmt=%~1"
set "line="
set "space=                                                                                                    "
setlocal enableDelayedExpansion
for %%n in (^"^

^") do for /f "tokens=1,2 delims=[" %%a in (".!fmt:]=%%~n.!") do (
  if "!!" equ "" endlocal
  set "const=%%a"
  call set "subst=%%~2%space%%%~2"
  setlocal enableDelayedExpansion
  if %%b0 geq 0 (set "subst=!subst:~0,%%b!") else set "subst=!subst:~%%b!"
  for /f delims^=^ eol^= %%c in ("!line!!const:~1!!subst!") do (
    endlocal
    set "line=%%c"
  )
  shift /2
)
setlocal enableDelayedExpansion
echo(!line!
exit /b