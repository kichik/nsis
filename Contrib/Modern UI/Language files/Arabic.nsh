;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Arabic (1025)
;Translation by asdfuae@msn.com
;updated by Rami Kattan

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ARABIC"

  !define MUI_LANGNAME "Arabic" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "ใัอศว ศ฿ Ýํ ใัิฯ ลฺฯวฯ $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "ำํำวฺฯ฿ ๅะว วแใัิฯ Ýํ สไีํศ $(^NameDA).\r\n\r\nใไ วแใÝึแ ลÛแวÞ ฬใํฺ วแศัวใฬ Þศแ วแสไีํศ. ำํำวฺฯ ๅะว Ýํ สฬฯํฯ ใแÝวส วแไูวใ ฯๆไ วแอวฬษ แลฺวฯษ สิÛํแ วแฬๅวา.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "ลสÝวÞํษþ วแสัฮํี"  
  !define MUI_TEXT_LICENSE_SUBTITLE "วแัฬวม ใัวฬฺษ ลสÝวÞํษþ วแสัฮํี Þศแ สไีํศ $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "ลึฺุ ใÝสวอ ีÝอษ แแรำÝแ แัฤํษ ศวÞํ วแลสÝวÞํษ"
  !define MUI_INNERTEXT_LICENSE_BOTTOM "ฺไฯ วแใๆวÝÞษ ฺแ์ ิัๆุ วแลสÝวÞํษþก ลึÛุ รๆวÝÞ แแใสวศฺษ. ํฬศ วแใๆวÝÞษ ฺแ์ วแลสÝวÞํษþ แสไีํศ $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "ฺไฯ วแใๆวÝÞษ ฺแ์ ิัๆุ วแลสÝวÞํษþก ฺแ๘ใ ใัศฺ วแฺแวใษ วแสวแํ. ํฬศ วแใๆวÝÞษ ฺแ์ วแลสÝวÞํษþ แสไีํศ $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "ฺไฯ วแใๆวÝÞษ ฺแ์ ิัๆุ วแลสÝวÞํษก ลฮสั วแฮํวั วแรๆแ ใไ วแสวแํ. ํฬศ วแใๆวÝÞษ ฺแ์ วแลสÝวÞํษ แสไีํศ $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "ลฮสั วแใ฿ๆไวส"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "ลฮสั ใําวส $(^NameDA) วแใัวฯ สไีํศๅว."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "วแๆีÝ"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "ริั ศวแÝรัษ ÝๆÞ รอฯ วแใ฿ๆไวส แัฤํษ วแๆีÝ"
  
  !define MUI_TEXT_DIRECTORY_TITLE "ลฮสั ใๆÞฺ วแสไีํศ"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "ลฮสั วแใฬแฯ วแใัวฯ สไีํศ $(^NameDA) Ýํๅ."
  
  !define MUI_TEXT_INSTALLING_TITLE "สไีํศ"
  !define MUI_TEXT_INSTALLING_SUBTITLE "วแัฬวม วแลไสูวั รหไวม สไีํศ $(^NameDA)."
  
  !define MUI_TEXT_FINISH_TITLE "ลไสๅ์ วแสไีํศ"
  !define MUI_TEXT_FINISH_SUBTITLE "ลไสๅส ฺใแํษ วแสไีํศ ศไฬวอ."
  
  !define MUI_TEXT_ABORT_TITLE "ลแÛวม วแสไีํศ"
  !define MUI_TEXT_ABORT_SUBTITLE "แใ ํไสๅํ วแสไีํศ ศไฬวอ."
  
  !define MUI_BUTTONTEXT_FINISH "&ลไๅวม"
  !define MUI_TEXT_FINISH_INFO_TITLE "ลไๅวม ใัิฯ ลฺฯวฯ $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "แÞฯ สใ สไีํศ $(^NameDA) ฺแ์ วแฬๅวา\r\n\r\nลึÛุ ลไๅวม แลÛแวÞ ใัิฯ วแลฺฯวฯ."
  !define MUI_TEXT_FINISH_INFO_REBOOT "ํฬศ ลฺวฯษ สิÛํแ วแฬๅวา แลไๅวม สไีํศ $(^NameDA). ๅแ สัํฯ ลฺวฯษ วแสิÛํแ วแยไฟ"
  !define MUI_TEXT_FINISH_REBOOTNOW "รฺฯ วแสิÛํแ วแยไ"
  !define MUI_TEXT_FINISH_REBOOTLATER "รัÛศ Ýํ ลฺวฯษ สิÛํแ วแฬๅวา Ýํ ๆÞส แวอÞ"
  !define MUI_TEXT_FINISH_RUN "&ิÛแ $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "วฺัึ& รÞัรไํ"
  
  !define MUI_TEXT_STARTMENU_TITLE "ลฮสั ใฬแฯ Þวฦใษ วศฯร"
  !define MUI_TEXT_STARTMENU_SUBTITLE "ลฮสั ใฬแฯ Þวฦใษ วศฯร แลฮสีวัวส $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "ลฮสั วแใฬแฯ Ýํ Þวฦใษ วศฯร วแะํ ำสไิร Ýํๅ ลฮสีวัวส วแศัไวใฬ. ํใ฿ไ รํึว ฿สวศษ ลำใ แลไิวม ใฬแฯ ฬฯํฯ."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "แว สไิฦ ลฮสีวัวส"
  
  !define MUI_TEXT_ABORTWARNING "ๅแ รไส ใสร฿ฯ ใไ ลÛแวÞ ใไี๘ศ $(^Name)ฟ"  
  

  !define MUI_UNTEXT_WELCOME_INFO_TITLE "ใัอศว ศ฿ Ýํ ใัิฯ ลาวแษ $(^NameDA) "  
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "ๅะว วแใัิฯ ำํฯแ๘฿ รหไวม ลาวแษ $(^NameDA).\r\n\r\n Þศแ วแศฯม ศวแลาวแษก ํัฬ์ วแสร฿ฯ ใไ รไ $(^NameDA) Ûํั ิÛ๘วแ.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "ลาวแษ $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "ลาวแษ $(^NameDA) ใไ วแฬๅวา."
  
  !define MUI_UNTEXT_LICENSE_TITLE "ลสÝวÞํษ วแสัฮํี"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "วแัฬวม ใัวฬฺษ ิัๆุ วแสัฮํี Þศแ ลาวแษ $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "ฺไฯ วแใๆวÝÞษ ฺแ์ ิัๆุ วแลสÝวÞํษก ลึÛุ ฺแ์ ใๆวÝÞ. ํฬศ วแใๆวÝÞษ ฺแ์ วแลสÝวÞํษ แลาวแษ $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "ฺไฯ วแใๆวÝÞษ ฺแ์ ิัๆุ วแลสÝวÞํษก ฺแ๘ใ วแใัศฺ วแฺแวใษ วแสวแํ. ํฬศ วแใๆวÝÞษ ฺแ์ วแลสÝวÞํษ แลาวแษ $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "ฺไฯ วแใๆวÝÞษ ฺแ์ ิัๆุ วแลสÝวÞํษก ลฮสั วแฮํวั วแรๆแ ใไ วแสวแํ. ํฬศ วแใๆวÝÞษ ฺแ์ วแลสÝวÞํษ แลาวแษ $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "ลฮสั วแใ฿ๆไวส"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "ลฮสั ใําวส $(^NameDA) วแใัวฯ ลาวแสๅว."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "ลฮสั ใๆÞฺ วแใาํแ"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "ลฮสั วแใฬแฯ วแะํ ำําวแ ใไๅ $(^NameDA)."  
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "ลาวแษ"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "วแัฬวม วแลไสูวั รหไวม ลาวแษ $(^NameDA)."
  
  !define MUI_UNTEXT_FINISH_TITLE "ลไสๅ์"
  !define MUI_UNTEXT_FINISH_SUBTITLE "ลไสๅส ฺใแํษ วแลาวแษ ศไฬวอ."
  
  !define MUI_UNTEXT_ABORT_TITLE "ลแÛวม วแลาวแษ"
  !define MUI_UNTEXT_ABORT_SUBTITLE "แใ สไสๅํ วแลาวแษ ศไฬวอ."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "ลไๅวม ใัิฯ ลาวแษ $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "แÞฯ สใ ลาวแษ $(^NameDA) ใไ วแฬๅวา.\r\n\r\n ลึÛุ ลไๅวม แลÛแวÞ วแใัิฯ."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "ํฬศ ลฺวฯษ สิÛํแ วแฬๅวา แลไๅวม ลาวแษ $(^NameDA). ๅแ สัํฯ ลฺวฯษ วแสิÛํแ วแยไฟ"
  
  !define MUI_UNTEXT_ABORTWARNING "ๅแ รไส ใสร฿ฯ ใไ รไ฿ วแฮัๆฬ ใไ ใาํแ $(^Name)ฟ"
  
!insertmacro MUI_LANGUAGEFILE_END