;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Dutch (1043)
;By Joost Verburg

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "DUTCH"

  !define MUI_LANGNAME "Nederlands" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Welkom bij de $(^Name) Setup Wizard"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Deze wizard zal $(^Name) op uw systeem installeren.\r\n\r\nHet is aanbevolen dat u alle andere programma's afsluit voordat u Setup start. Dit zorgt ervoor dat Setup bepaalde systeembestanden kan bijwerken zonder uw system opniew op te starten.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licentie Overeenkomst"
  !define MUI_TEXT_LICENSE_SUBTITLE "Lees de licentie overeenkomst voordat u $(^Name) installeert."
  !define MUI_INNERTEXT_LICENSE_TOP "Druk op Page Down om de rest van de overeenkomst te zien."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Klik op Akkoord op verder te gaan als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^Name) te installeren."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Klik op het keuzevak hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^Name) te installeren."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Selecteer de eerste optie hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^Name) te installeren."
  
  !define MUI_TEXT_COMPONENTS_TITLE "Kies Onderdelen"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Kies de onderdelen die u wilt installeren."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beschrijving"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beweeg uw muis over een onderdeel om een beschrijving te zien."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Kies Installatie Locatie"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Kies de map waarin u $(^Name) in wilt installeren."
  
  !define MUI_TEXT_INSTALLING_TITLE "Bezig met installeren"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Een ogenblik geduld a.u.b. terwijl $(^Name) wordt geinstalleerd."
  
  !define MUI_TEXT_FINISH_TITLE "Installatie Voltooid"
  !define MUI_TEXT_FINISH_SUBTITLE "De installatie is succesvol verlopen."
  
  !define MUI_TEXT_ABORT_TITLE "Installatie Afgebroken"
  !define MUI_TEXT_ABORT_SUBTITLE "De installatie is niet voltooid."
  
  !define MUI_BUTTONTEXT_FINISH "&Voltooien"
  !define MUI_TEXT_FINISH_INFO_TITLE "Voltooien van de $(^Name) Setup Wizard"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) is geinstalleerd op uw systeem.\r\n\r\nKlik op Voltooien om deze wizard te sluiten."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Uw system moet worden opnieuw opgestart om de installatie van $(^Name) te voltooien. Wilt u nu herstarten?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Nu herstarten"
  !define MUI_TEXT_FINISH_REBOOTLATER "Ik wil later handmatig herstarten"
  !define MUI_TEXT_FINISH_RUN "Start $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Leesmij weergeven"
  
  !define MUI_TEXT_STARTMENU_TITLE "Kies Start Menu Map"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Kies een map in het Startmenu voor de snelkoppelingen van $(^Name)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Kies een map in het Start Menu waarin de snelkoppelingen moeten worden aangemaakt. U kunt ook een naam invoeren om een niewe map te maken."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Geen snelkoppelingen aanmaken"
  
  !define MUI_TEXT_ABORTWARNING "Weet u zeker dat u $(^Name) Setup wilt afsluiten?"
  

  !define MUI_UNTEXT_CONFIRM_TITLE "Verwijder $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Verwijder $(^Name) van uw system."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licentie Overeenkomst"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Lees de licentie overeenkomst voordat u $(^Name) verwijdert."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Klik op Akkoord op verder te gaan als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^Name) te verwijderen."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Klik op het keuzevak hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^Name) te verwijderen."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Selecteer de eerste optie hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^Name) te verwijderen."
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Kies Onderdelen"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Kies de onderdelen die u wilt verwijderen."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Kies Deïnstallatie Locatie"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Kies de map waaruit u $(^Name) in wilt verwijderen."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Bezig met verwijderen"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Een ogenblik geduld a.u.b. terwijl $(^Name) van uw systeem wordt verwijderd."
  
  !define MUI_UNTEXT_FINISH_TITLE "Verwijderen Gereed"
  !define MUI_UNTEXT_FINISH_SUBTITLE "$(^Name) is verwijderd van uw systeem."
  
  !define MUI_UNTEXT_ABORT_TITLE "Verwijderen Afgebroken"
  !define MUI_UNTEXT_ABORT_SUBTITLE "$(^Name) is niet volledig verwijderd van uw systeem."
  
!insertmacro MUI_LANGUAGEFILE_END