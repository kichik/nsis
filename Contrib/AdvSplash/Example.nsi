Name "AdvSplash.dll test"

OutFile "AdvSplash Test.exe"

XPStyle on

Function .onInit
	# the plugins dir is automatically deleted when the installer exits
	InitPluginsDir
	File /oname=$PLUGINSDIR\splash.bmp "${NSISDIR}\Contrib\Makensisw\logo.bmp"
	#optional
	#File /oname=$PLUGINSDIR\splash.wav "C:\myprog\sound.wav"

	advsplash::show 1000 600 400 -1 $PLUGINSDIR\splash

	Pop $0 ; $0 has '1' if the user closed the splash screen early,
			; '0' if everything closed normal, and '-1' if some error occured.

	MessageBox MB_OK "Now with transparency"

	File /oname=$PLUGINSDIR\splash.bmp "${NSISDIR}\Contrib\Icons\modern.bmp"

	advsplash::show 1000 600 400 0xFF00FF $PLUGINSDIR\splash

	Pop $0 ; $0 has '1' if the user closed the splash screen early,
			; '0' if everything closed normal, and '-1' if some error occured.
FunctionEnd

Section
SectionEnd