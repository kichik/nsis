;Language: Dutch (1043)
;By Joost Verburg

!insertmacro LANGFILE "Dutch" "Nederlands"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Welkom bij de $(^NameDA) Setup Wizard"
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TEXT "Deze wizard zal $(^NameDA) op uw systeem installeren.$\r$\n$\r$\nHet is aanbevolen dat u alle andere programma's afsluit voordat u Setup start. Dit zorgt ervoor dat Setup bepaalde systeembestanden kan bijwerken zonder uw systeem opnieuw op te starten.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TITLE "Welkom bij de $(^NameDA) De-installatie Wizard"
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TEXT "Deze wizard zal $(^NameDA) van uw syteem verwijderen.$\r$\n$\r$\nControleer voordat u begint met verwijderen dat $(^NameDA) is afgesloten.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_WELCOMEPAGE | MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Voltooien"
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Licentie Overeenkomst"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Lees de licentie overeenkomst voordat u $(^NameDA) installeert."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Klik op Akkoord om verder te gaan als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te installeren."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Klik op het keuzevak hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te installeren."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Selecteer de eerste optie hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te installeren."
!endif

!ifdef MUI_UNLICENSEPAGE
  ${LangFileString} MUI_UNTEXT_LICENSE_TITLE "Licentie Overeenkomst"
  ${LangFileString} MUI_UNTEXT_LICENSE_SUBTITLE "Lees de licentie overeenkomst voordat u $(^NameDA) verwijdert."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM "Klik op Akkoord op verder te gaan als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te verwijderen."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Klik op het keuzevak hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te verwijderen."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Selecteer de eerste optie hieronder als u de overeenkomst accepteert. U moet de overeenkomst accepteren om $(^NameDA) te verwijderen."
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Druk op Page Down om de rest van de overeenkomst te zien."
!endif

!ifdef MUI_COMPONENTSPAGE
  ${LangFileString} MUI_TEXT_COMPONENTS_TITLE "Kies Onderdelen"
  ${LangFileString} MUI_TEXT_COMPONENTS_SUBTITLE "Kies de onderdelen die u wilt installeren."
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beschrijving"
!endif

!ifdef MUI_UNCOMPONENETSPAGE
  ${LangFileString} MUI_UNTEXT_COMPONENTS_TITLE "Kies Onderdelen"
  ${LangFileString} MUI_UNTEXT_COMPONENTS_SUBTITLE "Kies de onderdelen die u wilt verwijderen."
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beweeg uw muis over een onderdeel om de beschrijving te zien."
  !else
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beweeg uw muis over een onderdeel om de beschrijving te zien."
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  ${LangFileString} MUI_TEXT_DIRECTORY_TITLE "Kies Installatie Locatie"
  ${LangFileString} MUI_TEXT_DIRECTORY_SUBTITLE "Kies de map waarin u $(^NameDA) wilt installeren."
!endif

!ifdef MUI_UNDIRECTORYSPAGE
  ${LangFileString} MUI_UNTEXT_DIRECTORY_TITLE "Kies Locatie"
  ${LangFileString} MUI_UNTEXT_DIRECTORY_SUBTITLE "Kies de map waaruit u $(^NameDA) wilt verwijderen."
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Bezig met installeren"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Een ogenblik geduld a.u.b. terwijl $(^NameDA) wordt geinstalleerd."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Installatie Voltooid"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "De installatie is succesvol verlopen."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Installatie Afgebroken"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "De installatie is niet voltooid."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Bezig met verwijderen"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Een ogenblik geduld a.u.b. terwijl $(^NameDA) van uw systeem wordt verwijderd."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Verwijderen Gereed"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "$(^NameDA) is verwijderd van uw systeem."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Verwijderen Afgebroken"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "$(^NameDA) is niet volledig verwijderd van uw systeem."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Voltooien van de $(^NameDA) Setup Wizard"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) is geinstalleerd op uw systeem.$\r$\n$\r$\nKlik op Voltooien om deze wizard te sluiten."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Uw systeem moet opnieuw worden opgestart om de installatie van $(^NameDA) te voltooien. Wilt u nu herstarten?"
!endif

!ifdef MUI_UNFINISHPAGE
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TITLE "Voltooien van de $(^NameDA) De-installatie Wizard"
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) is verwijderd van uw systeem.$\r$\n$\r$\nKlik op Voltooien om deze wizard te sluiten."
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_REBOOT "Uw systeem moet opnieuw worden opgestart om het verwijderen $(^NameDA) te voltooien. Wilt u nu herstarten?"
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Nu herstarten"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Ik wil later handmatig herstarten"
  ${LangFileString} MUI_TEXT_FINISH_RUN "Start $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "Leesmij weergeven"
!endif

!ifdef MUI_STARTMENUPAGE
  ${LangFileString} MUI_TEXT_STARTMENU_TITLE "Kies Start Menu Map"
  ${LangFileString} MUI_TEXT_STARTMENU_SUBTITLE "Kies een map in het Start menu voor de snelkoppelingen van $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_TOP "Kies een map in het Start menu waarin de snelkoppelingen moeten worden aangemaakt. U kunt ook een naam invoeren om een nieuwe map te maken."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_CHECKBOX "Geen snelkoppelingen aanmaken"
!endif

!ifdef MUI_UNCONFIRMPAGE
  ${LangFileString} MUI_UNTEXT_CONFIRM_TITLE "Verwijder $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_CONFIRM_SUBTITLE "Verwijder $(^NameDA) van uw systeem."
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "Weet u zeker dat u $(^Name) Setup wilt afsluiten?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Weet u zeker dat u $(^Name) De-installatie wilt afsluiten?"
!endif
