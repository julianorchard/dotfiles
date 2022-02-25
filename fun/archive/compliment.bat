@echo off
:: Get a compliment from 
:: youaregreat.fyi (amazing domain name)

for /f "tokens=* USEBACKQ" %%f in (`curl -s -o nul -v https://youaregreat.fyi/api/`) do (
  set complimenter=%%f
)

set string=%complimenter:"=%
set "string=%string:~2,-2%"
set "string=%string:: ==%"
echo %string%
