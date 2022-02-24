@echo off

  git add .
 
    git commit -m "%*"


:: Empty 'message' variable
  set message=
  set force=

:: Loop through all arguments, 
:: skipping -f if it's found
  for %%a in (%*) do (
    if %%a==-f (
	echo Forcing push...
	set force=-f
    ) else (
	call set "message=%%message%% %%a"
    )
  )

      git commit -m "%message%"

    	git push %force% origin master
