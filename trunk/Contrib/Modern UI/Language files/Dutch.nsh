;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Dutch (1043)
;By Joost Verburg

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "DUTCH"

  !define MUI_LANGNAME "Nederlands" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Welkom bij de $(^NameDA) Setup Wizard"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Deze wizard zal $(^NameDA) op uw systeem installeren.\r\n\r\nHet is aanbevolen dat u alle andere programma's afsluit voordat u Setup start. Dit zorgt ervoor dat Setup bepaalde systeembestanden kan bijwerken zonder uw systeem opniew op te starten.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licentie Overeenkomst"
  !define MUI_TEXT_LICENSE_SUBTITLE "Lees de licentie overeenkomst voordat u $(^NameDA) installeert."
  !define MUI_INNERTEXT_LICENSE_TOP "Druk op Page Down om de rest van de overeenkomst te zien."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Klik op Akkoord op verder te gaan als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te installeren."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Klik op het keuzevak hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te installeren."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Selecteer de eerste optie hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te installeren."
  
  !define MUI_TEXT_COMPONENTS_TITLE "Kies Onderdelen"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Kies de onderdelen die u wilt installeren."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beschrijving"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beweeg uw muis over een onderdeel om een beschrijving te zien."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Kies Installatie Locatie"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Kies de map waarin u $(^NameDA) in wilt installeren."
  
  !define MUI_TEXT_INSTALLING_TITLE "Bezig met installeren"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Een ogenblik geduld a.u.b. terwijl $(^NameDA) wordt geinstalleerd."
  
  !define MUI_TEXT_FINISH_TITLE "Installatie Voltooid"
  !define MUI_TEXT_FINISH_SUBTITLE "De installatie is succesvol verlopen."
  
  !define MUI_TEXT_ABORT_TITLE "Installatie Afgebroken"
  !define MUI_TEXT_ABORT_SUBTITLE "De installatie is niet voltooid."
  
  !define MUI_BUTTONTEXT_FINISH "&Voltooien"
  !define MUI_TEXT_FINISH_INFO_TITLE "Voltooien van de $(^NameDA) Setup Wizard"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) is geinstalleerd op uw systeem.\r\n\r\nKlik op Voltooien om deze wizard te sluiten."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Uw systeem moet worden opnieuw opgestart om de installatie van $(^NameDA) te voltooien. Wilt u nu herstarten?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Nu herstarten"
  !define MUI_TEXT_FINISH_REBOOTLATER "Ik wil later handmatig herstarten"
  !define MUI_TEXT_FINISH_RUN "Start $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Leesmij weergeven"
  
  !define MUI_TEXT_STARTMENU_TITLE "Kies Start Menu Map"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Kies een map in het Startmenu voor de snelkoppelingen van $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Kies een map in het Start Menu waarin de snelkoppelingen moeten worden aangemaakt. U kunt ook een naam invoeren om een niewe map te maken."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Geen snelkoppelingen aanmaken"
  
  !define MUI_TEXT_ABORTWARNING "Weet u zeker dat u $(^Name) Setup wilt afsluiten?"
  

  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Welkom bij de $(^NameDA) De-installatie Wizard"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Deze wizard zal $(^NameDA) van uw syteem verwijderen.\r\n\r\nControleer voordat u begint met verwijderen dat $(^NameDA) is afgesloten.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Verwijder $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Verwijder $(^NameDA) van uw systeem."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licentie Overeenkomst"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Lees de licentie overeenkomst voordat u $(^NameDA) verwijdert."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Klik op Akkoord op verder te gaan als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te verwijderen."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Klik op het keuzevak hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te verwijderen."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Selecteer de eerste optie hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te verwijderen."
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Kies Onderdelen"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Kies de onderdelen die u wilt verwijderen."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Kies De-installatie Locatie"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Kies de map waaruit u $(^NameDA) wilt verwijderen."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Bezig met verwijderen"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Een ogenblik geduld a.u.b. terwijl $(^NameDA) van uw systeem wordt verwijderd."
  
  !define MUI_UNTEXT_FINISH_TITLE "De-installatie Gereed"
  !define MUI_UNTEXT_FINISH_SUBTITLE "$(^NameDA) is verwijderd van uw systeem."
  
  !define MUI_UNTEXT_ABORT_TITLE "De-installatie Afgebroken"
  !define MUI_UNTEXT_ABORT_SUBTITLE "$(^NameDA) is niet volledig verwijderd van uw systeem."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Voltooien van de $(^NameDA) De-installatie Wizard"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) is verwijderd van uw systeem.\r\n\r\nKlik op Voltooien om deze wizard te sluiten."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Uw systeem moet worden opnieuw opgestart om de de-installatie van $(^NameDA) te voltooien. Wilt u nu herstarten?"
  
  !define MUI_UNTEXT_ABORTWARNING "Weet u zeker dat u $(^Name) De-installatie wilt afsluiten?"
  
!insertmacro MUI_LANGUAGEFILE_END