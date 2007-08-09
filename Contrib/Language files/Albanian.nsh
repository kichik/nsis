;Language: Albanian (1052)
;Translation Besnik Bleta, besnik@spymac.com

!insertmacro LANGFILE "Albanian" "Albanian"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Mirësevini te Rregullimi i $(^NameDA)"
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TEXT "Ky do t'ju udhëheqë gjatë instalimit të $(^NameDA).$\r$\n$\r$\nKëshillohet që të mbyllni tërë zbatimet e tjerë para se të nisni Rregullimin. Kjo bën të mundur përditësim kartelash të rëndësishme sistemi pa u dashur të riniset kompjuteri juaj.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TITLE "Mirësevini te Çinstalimi i $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TEXT "Ky do t'ju udhëheqë gjatë çinstalimit të $(^NameDA).$\r$\n$\r$\nPara nisjes së çinstalimit, sigurohuni që $(^NameDA) nuk është duke xhiruar.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_WELCOMEPAGE | MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Përfundo"
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Marrëveshje License"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Ju lutem shqyrtoni kushtet e licensës përpara instalimit të $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Nëse pranoni kushtet e marrëveshjes, klikoni Pajtohem për të vazhduar. Duhet të pranoni marrëveshjen për të instaluar $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Nëse pranoni kushtet e marrëveshjes, klikoni kutizën më poshtë. Duhet të pranoni marrëveshjen për të instaluar $(^NameDA). $_CLICK"
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Nëse pranoni kushtet e marrëveshjes, përzgjidhni më poshtë mundësinë e parë. Duhet të pranoni marrëveshjen për të instaluar $(^NameDA). $_CLICK"
!endif

!ifdef MUI_UNLICENSEPAGE
  ${LangFileString} MUI_UNTEXT_LICENSE_TITLE "Marrëveshje License"
  ${LangFileString} MUI_UNTEXT_LICENSE_SUBTITLE "Ju lutem shqyrtoni kushtet e licensës përpara çinstalimit të $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM "Nëse pranoni kushtet e marrëveshjes, klikoni Pajtohem për të vazhduar. Duhet të pranoni marrëveshjen për të çinstaluar $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Nëse pranoni kushtet e marrëveshjes, klikoni kutizën më poshtë. Duhet të pranoni marrëveshjen për të çinstaluar $(^NameDA). $_CLICK"
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Nëse pranoni kushtet e marrëveshjes, përzgjidhni mundësinë e parë më poshtë. Duhet të pranoni marrëveshjen për të çinstaluar $(^NameDA). $_CLICK"
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Shtypni Page Down për të parë pjesën e mbetur të marrëveshjes."
!endif

!ifdef MUI_COMPONENTSPAGE
  ${LangFileString} MUI_TEXT_COMPONENTS_TITLE "Përzgjidhni Përbërës"
  ${LangFileString} MUI_TEXT_COMPONENTS_SUBTITLE "Zgjidhni cilat anë të $(^NameDA) doni të instalohen."
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Përshkrim"
!endif

!ifdef MUI_UNCOMPONENETSPAGE
  ${LangFileString} MUI_UNTEXT_COMPONENTS_TITLE "Zgjidhni Përbërësa"
  ${LangFileString} MUI_UNTEXT_COMPONENTS_SUBTITLE "Zgjidhni cilat anë të $(^NameDA) doni të çinstalohen."
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Për të parë përshkrimin e një përbërësi vendosni miun përsipër tij."
  !else
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Për të parë përshkrimin e një përbërësi vendosni miun përsipër tij."
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  ${LangFileString} MUI_TEXT_DIRECTORY_TITLE "Zgjidhni Vend Instalimi"
  ${LangFileString} MUI_TEXT_DIRECTORY_SUBTITLE "Zgjidhni dosjen tek e cila të instalohet $(^NameDA)."
!endif

!ifdef MUI_UNDIRECTORYSPAGE
  ${LangFileString} MUI_UNTEXT_DIRECTORY_TITLE "Zgjidhni Vend Çinstalimi"
  ${LangFileString} MUI_UNTEXT_DIRECTORY_SUBTITLE "Zgjidhni dosjen prej së cilës të instalohet $(^NameDA)."
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Po instaloj"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Ju lutem prisni ndërkohë që $(^NameDA) instalohet."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Instalim i Plotësuar"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Rregullimi u plotësua me sukses."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Instalimi u Ndërpre"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "Rregullimi nuk u plotësua me sukses."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Çinstalim"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Ju lutem prisni ndërsa $(^NameDA) çinstalohet."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Çinstalim i Plotë"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "Çinstalimi u plotësua me sukses."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Çinstalimi u Ndërpre"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "Çinstalimi nuk plotësua me sukses."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Po plotësoj Rregullimin e $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) u instalua në kompjuterin tuaj.$\r$\n$\r$\nKlikoni Përfundo për të mbyllur këtë proces."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Që të mund të plotësohet instalimi i $(^NameDA) kompjuteri juaj duhet të riniset. Doni ta rinisni tani?"
!endif

!ifdef MUI_UNFINISHPAGE
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TITLE "Po plotësoj çinstalimin e $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) u çinstalua prej kompjuterit tuaj.$\r$\n$\r$\nKlikoni Përfundo për të mbyllur procesin."
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_REBOOT "Kompjuteri juaj duhet të riniset që të mund të plotësohet çinstalimi i $(^NameDA). Doni ta rinisni tani?"
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Rinise tani"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Dua ta rinis dorazi më vonë"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Nis $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&Shfaq Readme"
!endif

!ifdef MUI_STARTMENUPAGE
  ${LangFileString} MUI_TEXT_STARTMENU_TITLE "Zgjidhni Dosje Menuje Start"
  ${LangFileString} MUI_TEXT_STARTMENU_SUBTITLE "Zgjidhni një dosje Menuje Start për shkurtprerje $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_TOP "Përzgjidhni dosjen Menu Start në të cilën do të donit të krijonit shkurtprerjet për programin. Mundeni edhe të jepni një emër për të krijuar një dosje të re."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_CHECKBOX "Mos krijo shkurtprerje"
!endif

!ifdef MUI_UNCONFIRMPAGE
  ${LangFileString} MUI_UNTEXT_CONFIRM_TITLE "Çinstalo $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_CONFIRM_SUBTITLE "Hiqni $(^NameDA) prej kompjuterit tuaj."
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "Jeni i sigurtë që doni të lini Rregullimin e $(^Name)?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Jeni i sigurtë që doni të lini Çinstalimin e $(^Name)?"
!endif
