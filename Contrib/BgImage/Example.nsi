Name "BgImage.dll test"

OutFile "BgImage Test.exe"

XPStyle on

Section
	BgImage::Init /NOUNLOAD /FILLSCREEN "${NSISDIR}\Contrib\Icons\modern.bmp"

	MessageBox MB_OK "Next image?"

	BgImage::SetImage /NOUNLOAD /FILLSCREEN "${NSISDIR}\Contrib\Icons\checks1.bmp"

	FindFirst $0 $1 $WINDIR\Media\*.wav
	StrCmp $0 "" skipSound
		moreSounds:
		StrCmp $1 "" noMoreSounds
			BgImage::Sound /NOUNLOAD $WINDIR\Media\$1
			# the sound is played asynchoronsly so NSIS can keep working while it plays
			# we don't want the message box to appear while playing so we give it a little time to play alone
			Sleep 500
			MessageBox MB_YESNO "Another sound?" IDNO noMoreSounds
				FindNext $0 $1
				Goto moreSounds

	noMoreSounds:
		FindClose $0
	skipSound:

	BgImage::Destroy
SectionEnd