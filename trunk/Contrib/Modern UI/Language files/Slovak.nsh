;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Slovak (1051)
;By Kypec (peter.dzugas@mahe.sk)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SLOVAK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Slovensky" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Vitajte v sprievodcovi inötal·cie programu $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Chyst·te sa nainötalovaù $(^Name) na svoj poËÌtaË.\r\n\r\nPred zaËiatkom inötal·cie je odpor˙ËanÈ zavrieù vöetky ostatnÈ aplik·cie. T˝mto umoûnÌte inötal·toru aktualizovaù prÌpadnÈ systÈmovÈ s˙bory bez nutnosti reötartovaù systÈm.\r\n\r\n$_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "LicenËn· zmluva"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Pred inötal·ciou programu $(^Name) si prosÌm preötudujte licenËnÈ podmienky."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "StlaËenÌm kl·vesy Page Down posuniete text licenËnej zmluvy."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ak s˙hlasÌte s podmienkami zmluvy, zvoæte S˙hlasÌm pre pokraËovanie. Je nutnÈ s˙hlasiù s licenËnou zmluvou, ak chcete pokraËovaù v inötal·cii $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ak s˙hlasÌte s podmienkami zmluvy, zaökrtnite niûöie uvedenÈ polÌËko. Je nutnÈ s˙hlasiù s licenËnou zmluvou, ak chcete pokraËovaù v inötal·cii $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ak s˙hlasÌte s podmienkami zmluvy, oznaËte prv˙ z niûöie uveden˝ch moûnostÌ. Je nutnÈ s˙hlasiù s licenËnou zmluvou, ak chcete pokraËovaù v inötal·cii $(^Name)."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Voæba komponentov"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Zvoæte komponenty programu $(^Name), ktorÈ chcete nainötalovaù."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Popis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pri pohybe myöou nad komponentom programu sa tu zobrazÌ jeho popis."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Voæba umiestnenia inötal·cie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Zvoæte adres·r, do ktorÈho chcete nainötalovaù program $(^Name)."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Inötal·cia"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "PoËkajte, prosÌm, na dokonËenie inötal·cie programu $(^Name)."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "DokonËenie inötal·cie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Inötal·cia prebehla v poriadku."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Inötal·cia bola preruöen·"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Inötal·cia nebola ˙speöne dokonËen·."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&DokonËiù"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "DokonËenie sprievodcu inötal·cie programu $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "Pr·ve ste nainötalovali program $(^Name) do svojho systÈmu.\r\nKliknite na DokonËiù pre uzavretie tohto sprievodcu."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Pre ˙plnÈ dokonËenie inötal·cie programu $(^Name) je nutnÈ vykonaù reötart V·öho systÈmu. Chcete reötartovaù ihneÔ?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Reötartovaù ihneÔ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Reötartovaù ruËne neskÙr"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Spusti $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "&Uk·û »Ìtaj-ma"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Voæba umiestnenia v ponuke ätart"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Zvoæte poloûku v ponuke ätart pre umiestnenie z·stupcov programu."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Vyberte poloûku v ponuke ätart, v ktorej chcete vytvoriù z·stupcov programu. Pokiaæ zad·te neexistuj˙cu poloûku, bude vytvoren· nov· s Vami zadan˝m menom."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nevytv·raù z·stupcov"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Naozaj chcete ukonËiù inötal·ciu programu $(^Name)?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Odinötalovanie programu $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Odstr·nenie programu $(^Name) z V·öho systÈmu."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Voæba komponentov"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Zvoæte komponenty programu $(^Name), ktorÈ chcete odinötalovaù."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Odinötalovanie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "PoËkajte, prosÌm, na dokonËenie odinötalovania programu $(^Name)."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "DokonËenÈ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Odinötalovanie prebehlo v poriadku."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Odinötal·cia bola preruöen·"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Odinötal·cia nebola ˙speöne dokonËen·."
  
!insertmacro MUI_LANGUAGEFILE_END