NSISdl 1.3 - HTTP downloading plugin for NSIS
---------------------------------------------

Copyright (C) 2001-2002 Yaroslav Faybishenko & Justin Frankel

This plugin can be used from NSIS to download files via http.

To connect to the internet, use the Dialer pluin.

USAGE
-----

NSISdl::download http://www.domain.com/file localfile.exe

You can also pass /TIMEOUT to set the timeout in milliseconds:

NSISdl::download /TIMEOUT=30000 http://www.domain.com/file localfile.exe

The return value is pushed to the stack:

  "cancel" if cancelled
  "success" if success
  otherwise, an error string describing the error

If you don't want the progess window to appear use NSISdl::download_quiet.

Example of usage:

NSISdl::download http://www.domain.com/file localfile.exe
Pop $R0 ;Get the return value
  StrCmp $R0 "success" +3
    MessageBox MB_OK "Download failed: $R0"
    Quit

For another example, see waplugin.nsi in the examples directory.

PROXIES
-------

NSISdl supports only basic configurations of proxies. It doesn't support
proxies which require authentication, automatic configuration script, etc.
NSISdl reads the proxy configuration from Internet Explorer's registry key
under HKLM\Software\Microsoft\Windows\CurrentVersion\Internet Settings. It
reads and parses ProxyEnable and ProxyServer.

If you don't want NSISdl to use Internet Explorer's settings, use the
/NOIEPROXY flag. /NOIEPROXY should be used after /TRANSLATE and
/TIMEOUT. For example:

NSISdl::download /NOIEPROXY http://www.domain.com/file localfile.exe
NSISdl::download /TIMEOUT=30000 /NOIEPROXY http://www.domain.com/file localfile.exe

TRANSLATE
---------

To translate NSISdl add the following values to the call line:

/TRANSLATE downloading connecting second minute hour plural progress remianing

Default values are:

  downloading - "Downloading %s"
  connecting - "Connecting ..."
  second - "second"
  minute - "minute"
  hour - "hour"
  plural - "s"
  progress - "%dkB (%d%%) of %dkB @ %d.%01dkB/s"
  remaining -  "(%d %s%s remaining)"

/TRANSLATE must come before /TIMEOUT.