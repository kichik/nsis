Name "UserInfo.dll test"
OutFile UserInfo.exe

Section
	UserInfo::GetName
	Pop $0
	UserInfo::GetAccountType
	Pop $1
	MessageBox MB_OK 'User "$0" in group "$1"'
SectionEnd