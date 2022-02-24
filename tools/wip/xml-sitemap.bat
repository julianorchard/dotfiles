@echo off

:: Recursive loop through folders and files
for /R %%f in ('dir /b') do (
  echo ^<url^>
  echo ^<loc^>%url%/%%f^<^/loc^>
  echo ^<^/url^>
)

