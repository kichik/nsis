;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.62

;Language: Slovak (1051)
;By Kypec (peter.kysucky@mahe.sk)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SLOVAK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Slovensky" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Kliknite na œalej pre pokraËovanie."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Kliknite na Inötalovaù pre spustenie inötal·cie."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Vitajte v sprievodcovi inötal·cie programu ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Chyst·te sa nainötalovaù ${MUI_PRODUCT} na svoj poËÌtaË.\r\n\r\nPred zaËiatkom inötal·cie je odpor˙ËanÈ zavrieù vöetky ostatnÈ aplik·cie. T˝mto umoûnÌte inötal·toru aktualizovaù prÌpadnÈ systÈmovÈ s˙bory bez nutnosti reötartovaù systÈm.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "LicenËn· zmluva"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Pred inötal·ciou programu ${MUI_PRODUCT}, prosÌm, preötudujte licenËnÈ podmienky."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "StlaËenÌm kl·vesy Page Down posuniete text licenËnej zmluvy."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ak s˙hlasÌte so vöetk˝mi podmienkami zmluvy, zvoæte S˙hlasÌm pre pokraËovanie. Je nutnÈ s˙hlasiù s licenËnou zmluvou pre inötal·ciu programu ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Voæba komponentov"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Zvoæte komponenty programu ${MUI_PRODUCT}, ktorÈ chcete nainötalovaù."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Zaökrtnite tie komponenty, ktorÈ chcete nainötalovaù a odökrtnite tie, ktorÈ nechcete."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Popis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pri pohybe myöou nad komponentom programu sa tu zobrazÌ jeho popis."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Voæba umiestnenia inötal·cie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Zvoæte adres·r, do ktorÈho chcete nainötalovaù program ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} bude nainötalovan˝ do nasleduj˙ceho adres·ra.$\r$\n$\r$\nKliknite na Prehliadaù, pokiaæ chcete tento adres·r zmeniù."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Cieæov˝ adres·r"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Inötal·cia"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "PoËkajte, prosÌm, na dokonËenie inötal·cie programu ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "DokonËenie inötal·cie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Inötal·cia prebehla v poriadku."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": dokonËenÈ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&DokonËiù"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "DokonËenie sprievodcu inötal·cie programu ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "Pr·ve ste nainötalovali program ${MUI_PRODUCT} do svojho systÈmu.\r\nKliknite na DokonËiù pre uzavretie tohto sprievodcu."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Pre ˙plnÈ dokonËenie inötal·cie programu ${MUI_PRODUCT} je nutnÈ vykonaù reötart V·öho systÈmu. Chcete reötartovaù ihneÔ?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Reötartovaù ihneÔ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Reötartovaù ruËne neskÙr"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Spustiù ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Uk·zaù »Ìtaj-ma"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": Ponuka ätart"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Voæba umiestnenia v ponuke ätart"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Zvoæte poloûku v ponuke ätart pre umiestnenie z·stupcov programu."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Vyberte poloûku v ponuke ätart, v ktorej chcete vytvoriù z·stupcov programu. Pokiaæ zad·te neexistuj˙cu poloûku, bude vytvoren· nov· s Vami zadan˝m menom."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nevytv·raù z·stupcov"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Naozaj chcete ukonËiù inötal·ciu programu ${MUI_PRODUCT}?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Kliknite na Odinötalovaù pre spustenie odinötal·cie."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Odinötalovanie programu ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Odstr·nenie programu ${MUI_PRODUCT} z V·öho systÈmu."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "T˝mto odinötalujete program ${MUI_PRODUCT} z V·öho systÈmu."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Odinötalovanie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "PoËkajte, prosÌm, na dokonËenie odinötalovania programu ${MUI_PRODUCT}."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "DokonËenÈ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Odinötalovanie prebehlo v poriadku."
  
!insertmacro MUI_LANGUAGEFILE_END