@echo off

:::   mklist.bat  ---  List folder contents into a file.

:: Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

::: Description:

:: I use this surprisingly frequently. 

:: Probably not best practice because using DIR ouput (which 
:: I *assume* would run into similar issues to doing the same with
:: bash, which I know is definitely not good practice).

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

dir /b /a-d>>"folder-contents.txt"
