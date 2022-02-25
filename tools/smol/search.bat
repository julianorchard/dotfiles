@echo off

set input=%1
shift
:: Displays a bare list of all
:: files in a drive recursively 
dir "%input%*" /s/q/b
