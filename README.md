# **Unofficial** "Nullsoft Scriptable Install System" (NSIS) builds

Original project's website: https://nsis.sourceforge.io/Main_Page<br>
Original project's GitHub page: https://github.com/kichik/nsis<br>

[![License: zlib/libpng](https://img.shields.io/badge/License-zlib%2Flibpng-blue.svg)](http://nsis.sourceforge.net/License)
[![Latest Release](https://img.shields.io/badge/dynamic/json.svg?label=Latest%20Release&url=https%3A%2F%2Fapi.github.com%2Frepos%2Fnegrutiu%2Fnsis%2Freleases%2Flatest&query=%24.name&colorB=orange)](../../releases/latest)
[![Downloads](https://img.shields.io/github/downloads/negrutiu/nsis/total.svg?label=Downloads&colorB=orange)](../../releases/latest)
[![GitHub issues](https://img.shields.io/github/issues/negrutiu/nsis.svg?label=Issues)](../../issues)

This project was started mainly to offer you early access to the NSIS 64-bit features.<br>
I've also included a bunch of extra plugins for you to play with... Enjoy!

### Features:
* Native **x86** and **amd64** NSIS compilers
* Can produce native **x86** and **amd64** installers, compatible with all Windows versions (NT4+)
* Extra builtin plugins:
  * [NScurl](https://github.com/negrutiu/nsis-nscurl) - Plugin with advanced HTTP/S capabilities. Useful for file transfers, REST API calls, etc.
  * [NSxfer](https://github.com/negrutiu/nsis-nsxfer) - Plugin with advanced HTTP/S capabilities. (Consider switching to NScurl)
  * [NSutils](https://github.com/negrutiu/nsis-nsutils) - Plugin with multiple goodies packed in one basket
  * [ExecDos](https://github.com/negrutiu/nsis-execdos) - Extended support for launching child processes
  * [TaskbarProgress](https://github.com/negrutiu/nsis-taskbarprogress) - Display progress in Windows' taskbar
  * [ShellLink](https://github.com/negrutiu/nsis-shelllink) - Complex operations with shortcuts (`*.lnk`)
* Two new extra-large UI themes [ModernXL](https://github.com/negrutiu/nsis/wiki/ModernXL/) and [ModernXXL](https://github.com/negrutiu/nsis/wiki/ModernXL/)
* Advanced logging enabled (`NSIS_CONFIG_LOG`)
* Large strings (`NSIS_MAX_STRLEN=4096`)
