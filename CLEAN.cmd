if exist dd.bat del dd.bat
chcp 1251
dir /s/b vip7*.dll >>dd.bat
dir /s/b *.obj >>dd.bat
rem dir /s/b *.exe >>dd.bat
dir /s/b *.map >>dd.bat
dir /s/b *.res >>dd.bat
dir /s/b *.options >>dd.bat
dir /s/b *.bro >>dd.bat
dir /s/b *.rlt >>dd.bat
dir /s/b *.rc >>dd.bat
dir /s/b *.deb >>dd.bat
dir /s/b *.scope >>dd.bat
dir /s/b *.scopeinfo >>dd.bat
dir /s/b *.tmp >>dd.bat
dir /s/b $*.* >>dd.bat
dir /s/b @*.* >>dd.bat
dir /s/b capdos.* >>dd.bat
dir /s/b dd.* >>dd.bat
..\..\SpbVipTools\bin\delfiles.exe dd.bat
del dd.bat
pause
