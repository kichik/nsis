Name "System.dll Test"

OutFile "System.dll Test.exe"

Section
	; Memory for paths
	System::Alloc 1024
	Pop $1
	; Get drives
	System::Call 'kernel32::GetLogicalDriveStringsA(i 1024, i r1) i'
	enumok:
		; One more drive?
		System::Call 'kernel32::lstrlenA(i r1) i.r2'
		IntCmp $2 0 enumex

		; Drive space
		System::Call 'kernel32::GetDiskFreeSpaceExA(i r1, *l .r3, *l .r4, *l .r5) i'

		; Pretty KBs
		System::Int64Op $3 / 1024
		Pop $3
		System::Int64Op $4 / 1024
		Pop $4
		System::Int64Op $5 / 1024
		Pop $5

		; Get pretty drive path string
		System::Call '*$1(&t1024 .r6)'
		; Message box
		System::Call 'user32::MessageBoxA(i $HWNDPARENT, t "Path: `$6`, Free for Caller: $3 kb, Free for All: $5 kb, Disk Size: $4 kb$\r$\nMore?", t "Disk spaces example", i 33) i.r0'
		; User cancel?
		IntCmp $0 2 enumex

		; Next drive path
		IntOp $1 $1 + $2
		IntOp $1 $1 + 1
		goto enumok

	enumex: ; End of drives or user cancel
		; Free memory for paths
		System::Free $1
SectionEnd