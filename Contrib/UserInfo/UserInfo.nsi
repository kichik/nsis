Name "UserInfo.dll test"
OutFile UserInfo.exe

Section
	UserInfo::GetName
	Pop $0
	UserInfo::GetAccountType
	Pop $1
	StrCmp $1 "Admin" 0 +3
		MessageBox MB_OK 'User "$0" is in the Administrators group'
		Goto done
	StrCmp $1 "Power" 0 +3
		MessageBox MB_OK 'User "$0" is in the Power Users group'
		Goto done
	StrCmp $1 "User" 0 +3
		MessageBox MB_OK 'User "$0" is just a regular user'
		Goto done
	StrCmp $1 "Guest" 0 +3
		MessageBox MB_OK 'User "$0" is a guest'
		Goto done
	MessageBox MB_OK "Error! This DLL can't run under Windows 9x!"

	done:
SectionEnd