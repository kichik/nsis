# NSIS

[![License: zlib/libpng](https://img.shields.io/badge/License-zlib%2Flibpng-blue.svg)](http://nsis.sourceforge.net/License)
[![Latest Release](https://img.shields.io/badge/dynamic/json.svg?label=Latest%20Release&url=https%3A%2F%2Fapi.github.com%2Frepos%2Fnegrutiu%2Fnsis%2Freleases%2Flatest&query=%24.name&colorB=orange)](../../releases/latest)
[![Downloads](https://img.shields.io/github/downloads/negrutiu/nsis/total.svg?label=Downloads&colorB=orange)](../../releases/latest)
[![GitHub issues](https://img.shields.io/github/issues/negrutiu/nsis.svg?label=Issues)](../../issues)

### Unofficial builds of the wonderful [NSIS project](http://nsis.sourceforge.net/Main_Page)

Features:
* Native **x86** and **amd64** NSIS compiler
* Can produce native **x86** and **amd64** installers, compatible with all Windows versions (NT4+)
* Extra builtin plugins:
  * [ExecDos](https://github.com/negrutiu/nsis-execdos) - Extended support for launching child processes
  * [NSutils](https://github.com/negrutiu/nsis-nsutils) - Plugin with multiple goodies packed in one basket
  * [NSxfer](https://github.com/negrutiu/nsis-nsxfer) - Plugin with advanced HTTP/S support. Useful for file transfers, REST API calls, etc.
  * [TaskbarProgress](https://github.com/negrutiu/nsis-taskbarprogress) - Display progress in Windows' taskbar
  * [ShellLink](https://github.com/negrutiu/nsis-shelllink) - Complex operations with shortcuts (`*.lnk`)
* Advanced logging enabled (`NSIS_CONFIG_LOG`)
* Large strings (`NSIS_MAX_STRLEN=4096`)
