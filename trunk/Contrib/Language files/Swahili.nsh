;Language: Swahili (1089)

!insertmacro LANGFILE "Swahili" = "Kiswahili" =

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Karibu kwenye $(^NameDA) Usanidi"
  #FIXME:MUI_TEXT_WELCOME_INFO_TEXT 
!endif

!ifdef MUI_UNWELCOMEPAGE
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TITLE 
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TEXT "
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Mapatano ya Leseni"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Tafadhali soma matakwa ya leseni kabla ya kusakinisha (kuweka kwenye kompyuta) $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Ikiwa unakubali matakwa ya mapatano, bofya Nakubali ili kuendelea. Lazima ukubali mapatano kabla ya kusakinisha (kuweka kwenye kompyuta) $(^NameDA)."
  #FIXME:MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX 
  #FIXME:MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS
!endif

!ifdef MUI_UNLICENSEPAGE
  #FIXME:MUI_UNTEXT_LICENSE_TITLE 
  #FIXME:MUI_UNTEXT_LICENSE_SUBTITLE 
  #FIXME:MUI_UNINNERTEXT_LICENSE_BOTTOM 
  #FIXME:MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX 
  #FIXME:MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS 
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Bofya Page Down uone sehemu inayosalia ya mapatano."
!endif

!ifdef MUI_COMPONENTSPAGE
  #FIXME:MUI_TEXT_COMPONENTS_TITLE 
  #FIXME:MUI_TEXT_COMPONENTS_SUBTITLE 
!endif

!ifdef MUI_UNCOMPONENTSPAGE
  #FIXME:MUI_UNTEXT_COMPONENTS_TITLE 
  #FIXME:MUI_UNTEXT_COMPONENTS_SUBTITLE 
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Ufafanuzi"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    #FIXME:MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO 
  !else
    #FIXME:MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO 
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  #FIXME:MUI_TEXT_DIRECTORY_TITLE 
  #FIXME:MUI_TEXT_DIRECTORY_SUBTITLE 
!endif

!ifdef MUI_UNDIRECTORYPAGE
  #FIXME:MUI_UNTEXT_DIRECTORY_TITLE 
  #FIXME:MUI_UNTEXT_DIRECTORY_SUBTITLE 
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Inasakinisha"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Tafadhali subiri, $(^NameDA) inasakinishwa."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Usakinishaji Umekamilika"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Usanidi umekamilika ifaavyo."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Installation Aborted"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "SUsanidi haukukamilika ifaavyo."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Inasakinusha"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Tafadhali subiri, $(^NameDA) inasakinushwa."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Usakinushaji Umekamilika"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "Usakinushaji umekamilika ifaavyo."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Usakinushaji Umekatizwa "
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "Usakinushaji haukukamilika ifaavyo."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Inamalizia Usanidi wa $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) imesakinishwa katika kompyuta yako.$\r$\n$\r$\nBofya Maliza ili kufunga Usanidi."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Lazima kompyuta yako iwashwe upya ili kukamilisha usakinishaji wa $(^NameDA). Ungependa iwashwe upya sasa?"
!endif

!ifdef MUI_UNFINISHPAGE
  #FIXME:MUI_UNTEXT_FINISH_INFO_TITLE 
  #FIXME:MUI_UNTEXT_FINISH_INFO_TEXT 
  #FIXME:MUI_UNTEXT_FINISH_INFO_REBOOT 
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Washa upya sasa"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Nataka kuiwasha upya baadaye"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Endesha $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&Onyesha faili yenye maagizo"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Maliza"  
!endif

!ifdef MUI_STARTMENUPAGE
  #FIXME:MUI_TEXT_STARTMENU_TITLE 
  #FIXME:MUI_TEXT_STARTMENU_SUBTITLE 
  #FIXME:MUI_INNERTEXT_STARTMENU_TOP 
  #FIXME:MUI_INNERTEXT_STARTMENU_CHECKBOX 
!endif

!ifdef MUI_UNCONFIRMPAGE
  #FIXME:MUI_UNTEXT_CONFIRM_TITLE 
  #FIXME:MUI_UNTEXT_CONFIRM_SUBTITLE 
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "Una hakika kwamba unataka kuaga Usanidi wa $(^Name) ?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Una hakika kwamba unataka kuaga Usakinushaji wa $(^Name)?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_TITLE
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_SUBTITLE 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_TOP 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER 
!endif
