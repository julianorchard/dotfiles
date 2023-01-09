@echo off

:::   config.bat  ---  Managing the bare dotfiles repo when in Windows.

:: Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

::: Description:

:: The command used to manage the bare repo:

:: ```cmd
:: c:/Program Files/Git/mingw64/bin/git.exe
:: --git-dir=%HOME%/.dotfiles/ --work-tree=%HOME%
:: ```

:: ... for use in the Windows CMD.

::: License:

:: See /LICENSE file in the root of this repository.

::: Code:

git --git-dir=%HOME%\.dotfiles\ --work-tree=%HOME% %*
