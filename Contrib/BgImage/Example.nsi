Name "BgImage.dll test"

OutFile "BgImage Test.exe"

XPStyle on

Section
	# the plugins dir is automatically deleted when the installer exits
	InitPluginsDir
	File /oname=$PLUGINSDIR\modern.bmp "${NSISDIR}\Contrib\Icons\modern.bmp"
	File /oname=$PLUGINSDIR\checks1.bmp "${NSISDIR}\Contrib\Icons\checks1.bmp"

	BgImage::Init /NOUNLOAD /FILLSCREEN $PLUGINSDIR\modern.bmp

	MessageBox MB_OK "Next image?"

	BgImage::SetImage /NOUNLOAD /FILLSCREEN $PLUGINSDIR\checks1.bmp

	FindFirst $0 $1 $WINDIR\Media\*.wav
	StrCmp $0 "" skipSound
		moreSounds:
		StrCmp $1 "" noMoreSounds
			BgImage::Sound /NOUNLOAD /WAIT $WINDIR\Media\$1
			MessageBox MB_YESNO "Another sound?" IDNO noMoreSounds
				FindNext $0 $1
				Goto moreSounds

	noMoreSounds:
		FindClose $0
	skipSound:

	BgImage::Destroy
SectionEnd