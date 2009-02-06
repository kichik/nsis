OutFile icon2.exe
Name icon2

Icon "${NSISDIR}\Contrib\Graphics\Icons\nsis1-install.ico"
UninstallIcon "${NSISDIR}\Contrib\Graphics\Icons\arrow-install.ico"

Section
Return
WriteUninstaller $TEMP\uninst.exe
SectionEnd

Section uninstall
SectionEnd
