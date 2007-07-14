;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Afrikaans (1078)
;By Friedel Wolff

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Afrikaans"

  !define MUI_LANGNAME "Afrikaans" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Welkom by die $(^NameDA) Installasieslimmerd"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Hierdie slimmerd lei mens deur die installasie van $(^NameDA).\r\n\r\nDit word aanbeveel dat u alle ander programme afsluit voor die begin van die installasie. Dit maak dit moontlik om die relevante stelsellêers op te dateer sonder om die rekenaar te herlaai.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Lisensie-ooreenkoms"
  !define MUI_TEXT_LICENSE_SUBTITLE "Lees die lisensie-ooreenkoms voordat u $(^NameDA) installeer."
  !define MUI_INNERTEXT_LICENSE_TOP "Druk op Page Down om die res van die ooreenkoms te sien."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Klik op Regso om verder te gaan as u die ooreenkoms aanvaar. U moet die ooreenkoms aanvaar om $(^NameDA) te installeer."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Merk die blokkie hier onder as u die ooreenkoms aanvaar. U moet die ooreenkoms aanvaar om $(^NameDA) te installeer. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Kies die eerste keuse hieronder as u die ooreenkoms aanvaar. U moet die ooreenkoms aanvaar om $(^NameDA) te installeer. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Kies komponente"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Kies watter komponente van $(^NameDA) geïnstalleer moet word."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrywing"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beweeg die muis oor 'n komponent om sy beskrywing te sien."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Kies 'n komponent om sy beskrywing te sien."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Kies installasieplek"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Kies die gids waarin u $(^NameDA) wil installeer."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installeer tans"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Wag asb. terwyl $(^NameDA) geïnstalleer word."
  
  !define MUI_TEXT_FINISH_TITLE "Installasie voltooid"
  !define MUI_TEXT_FINISH_SUBTITLE "Die installasie is suksesvol voltooi."
  
  !define MUI_TEXT_ABORT_TITLE "Installasie gestaak"
  !define MUI_TEXT_ABORT_SUBTITLE "Die installasie is nie suksesvol voltooi nie."
  
  !define MUI_BUTTONTEXT_FINISH "&Voltooi"
  !define MUI_TEXT_FINISH_INFO_TITLE "Voltooi van die $(^NameDA) Installasieslimmerd"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) is geïnstalleer op uw rekenaar.\r\n\r\nKlik op Voltooi om hierdie slimmerd af te sluit."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Die rekenaar moet oorbegin word om die installasie van $(^NameDA) te voltooi. Wil u nou oorbegin?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Begin nou oor"
  !define MUI_TEXT_FINISH_REBOOTLATER "Ek wil later self oorbegin"
  !define MUI_TEXT_FINISH_RUN "&Laat loop $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Wys Leesmy-lêer"
  
  !define MUI_TEXT_STARTMENU_TITLE "Kies gids in Begin-kieslys"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Kies 'n gids in die Begin-kieslys vir $(^NameDA) se kortpaaie."
  !define MUI_INNERTEXT_STARTMENU_TOP "Kies die gids in die Begin-kieslys waarin die program se kortpaaie geskep moet word. U kan ook 'n nuwe naam gee om 'n nuwe gids te skep."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Moenie kortpaaie maak nie"
  
  !define MUI_TEXT_ABORTWARNING "Wil u definitief die installasie van $(^Name) afsluit?"
  

  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Welkom by die $(^NameDA) Verwyderingslimmerd"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Hierdie slimmerd lei mens deur die verwydering van $(^NameDA).\r\n\r\nVoor die verwydering begin word, maak seker dat $(^NameDA) nie loop nie.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Verwyder $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Verwyder $(^NameDA) van u rekenaar."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Lisensie-ooreenkoms"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Lees die lisensie-ooreenkoms voordat u $(^NameDA) verwyder."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Klik op Regso om verder te gaan as u die ooreenkoms aanvaar. U moet die ooreenkoms aanvaar om $(^NameDA) te verwyder."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Merk die kiesblokkie hieronder as u die ooreenkoms aanvaar. U moet die ooreenkoms aanvaar om $(^NameDA) te verwyder."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Kies die eerste keuse hieronder as u die ooreenkoms aanvaar. U moet die ooreenkoms aanvaar om $(^NameDA) te verwyder."
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Kies komponente"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Kies watter komponente van $(^NameDA) verwyder moet word."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Kies verwyderinggids"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Kies die gids waaruit u $(^NameDA) wil verwyder."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Verwyder tans"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Wag asb. terwyl $(^NameDA) van u rekenaar verwyder word."
  
  !define MUI_UNTEXT_FINISH_TITLE "Verwydering voltooi"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Verwydering is suksesvol voltooi."
  
  !define MUI_UNTEXT_ABORT_TITLE "Verwydering gestaak"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Verwydering is nie suksesvol voltooi nie."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Voltooi van die $(^NameDA) Verwyderingslimmerd"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) is van u rekenaar verwyder.\r\n\r\nKlik op Voltooi om hierdie slimmerd af te sluit."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Die rekenaar moet oorbegin word om die verwydering van $(^NameDA) te voltooi. Wil u nou oorbegin?"
  
  !define MUI_UNTEXT_ABORTWARNING "Wil u definitief die verwydering van $(^Name) afsluit?"
  
!insertmacro MUI_LANGUAGEFILE_END
