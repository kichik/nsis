OutFile icon1.exe
Name icon1

Icon "${NSISDIR}\Contrib\Graphics\Icons\arrow-install.ico"
UninstallIcon "${NSISDIR}\Contrib\Graphics\Icons\nsis1-install.ico"

Section
Return
WriteUninstaller $TEMP\uninst.exe
SectionEnd

Section uninstall
SectionEnd
