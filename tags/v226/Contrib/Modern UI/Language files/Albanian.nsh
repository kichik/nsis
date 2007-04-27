;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Albanian (1052)
;Translation Besnik Bleta, besnik@spymac.com

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Albanian"

  !define MUI_LANGNAME "Albanian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Mirësevini te Rregullimi i $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ky do t'ju udhëheqë gjatë instalimit të $(^NameDA).\r\n\r\nKëshillohet që të mbyllni tërë zbatimet e tjerë para se të nisni Rregullimin. Kjo bën të mundur përditësim kartelash të rëndësishme sistemi pa u dashur të riniset kompjuteri juaj.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Marrëveshje License"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Ju lutem shqyrtoni kushtet e licensës përpara instalimit të $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Shtypni Page Down për të parë pjesën e mbetur të marrëveshjes."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Nëse pranoni kushtet e marrëveshjes, klikoni Pajtohem për të vazhduar. Duhet të pranoni marrëveshjen për të instaluar $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Nëse pranoni kushtet e marrëveshjes, klikoni kutizën më poshtë. Duhet të pranoni marrëveshjen për të instaluar $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Nëse pranoni kushtet e marrëveshjes, përzgjidhni më poshtë mundësinë e parë. Duhet të pranoni marrëveshjen për të instaluar $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Përzgjidhni Përbërës"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Zgjidhni cilat anë të $(^NameDA) doni të instalohen."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Përshkrim"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Për të parë përshkrimin e një përbërësi vendosni miun përsipër tij."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Zgjidhni Vend Instalimi"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Zgjidhni dosjen tek e cila të instalohet $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Po instaloj"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Ju lutem prisni ndërkohë që $(^NameDA) instalohet."
  
  !define MUI_TEXT_FINISH_TITLE "Instalim i Plotësuar"
  !define MUI_TEXT_FINISH_SUBTITLE "Rregullimi u plotësua me sukses."
  
  !define MUI_TEXT_ABORT_TITLE "Instalimi u Ndërpre"
  !define MUI_TEXT_ABORT_SUBTITLE "Rregullimi nuk u plotësua me sukses."
  
  !define MUI_BUTTONTEXT_FINISH "&Përfundo"
  !define MUI_TEXT_FINISH_INFO_TITLE "Po plotësoj Rregullimin e $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) u instalua në kompjuterin tuaj.\r\n\r\nKlikoni Përfundo për të mbyllur këtë proces."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Që të mund të plotësohet instalimi i $(^NameDA) kompjuteri juaj duhet të riniset. Doni ta rinisni tani?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Rinise tani"
  !define MUI_TEXT_FINISH_REBOOTLATER "Dua ta rinis dorazi më vonë"
  !define MUI_TEXT_FINISH_RUN "&Nis $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Shfaq Readme"
  
  !define MUI_TEXT_STARTMENU_TITLE "Zgjidhni Dosje Menuje Start"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Zgjidhni një dosje Menuje Start për shkurtprerje $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Përzgjidhni dosjen Menu Start në të cilën do të donit të krijonit shkurtprerjet për programin. Mundeni edhe të jepni një emër për të krijuar një dosje të re."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Mos krijo shkurtprerje"
  
  !define MUI_TEXT_ABORTWARNING "Jeni i sigurtë që doni të lini Rregullimin e $(^Name)?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Mirësevini te Çinstalimi i $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ky do t'ju udhëheqë gjatë çinstalimit të $(^NameDA).\r\n\r\nPara nisjes së çinstalimit, sigurohuni që $(^NameDA) nuk është duke xhiruar.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Çinstalo $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Hiqni $(^NameDA) prej kompjuterit tuaj."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Marrëveshje License"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Ju lutem shqyrtoni kushtet e licensës përpara çinstalimit të $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Nëse pranoni kushtet e marrëveshjes, klikoni Pajtohem për të vazhduar. Duhet të pranoni marrëveshjen për të çinstaluar $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Nëse pranoni kushtet e marrëveshjes, klikoni kutizën më poshtë. Duhet të pranoni marrëveshjen për të çinstaluar $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Nëse pranoni kushtet e marrëveshjes, përzgjidhni mundësinë e parë më poshtë. Duhet të pranoni marrëveshjen për të çinstaluar $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Zgjidhni Përbërësa"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Zgjidhni cilat anë të $(^NameDA) doni të çinstalohen."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Zgjidhni Vend Çinstalimi"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Zgjidhni dosjen prej së cilës të instalohet $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Çinstalim"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Ju lutem prisni ndërsa $(^NameDA) çinstalohet."
    
  !define MUI_UNTEXT_FINISH_TITLE "Çinstalim i Plotë"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Çinstalimi u plotësua me sukses."
  
  !define MUI_UNTEXT_ABORT_TITLE "Çinstalimi u Ndërpre"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Çinstalimi nuk plotësua me sukses."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Po plotësoj çinstalimin e $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) u çinstalua prej kompjuterit tuaj.\r\n\r\nKlikoni Përfundo për të mbyllur procesin."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Kompjuteri juaj duhet të riniset që të mund të plotësohet çinstalimi i $(^NameDA). Doni ta rinisni tani?"
  
  !define MUI_UNTEXT_ABORTWARNING "Jeni i sigurtë që doni të lini Çinstalimin e $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END
