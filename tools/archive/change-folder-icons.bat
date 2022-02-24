if [%1] == [] goto:eof
echo [.ShellClassInfo] >%1\desktop.in
echo IconResource="C:\Users\%username%\Documents\Personal Admin\ICO\file-manager-blue.ico",0 >>%1\desktop.in
move %1\desktop.in %1\desktop.ini
attrib +S +H %1\desktop.ini
attrib +R %1
