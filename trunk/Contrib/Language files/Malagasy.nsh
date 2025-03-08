;Language: Malagasy (1033)

!insertmacro LANGFILE "Malagasy" = "Malagasy" =

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Tonga soa eto Amin'ny Fampidirana ny $(^NameDA)"
  #FIXME:MUI_TEXT_WELCOME_INFO_TEXT 
!endif

!ifdef MUI_UNWELCOMEPAGE
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TITLE 
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TEXT 
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Fifanekena"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Jereo aloha izay voalaza ao amin'ny fifanekena, alohan'ny hampidirana ny $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Raha ekenao izay voalaza ao amin'ny fifanekena ary te hanohy ianao, dia tsindrio ny Ekeko. Tsy maintsy manaiky an'io fifanekena io ianao vao afaka mampiditra ny $(^NameDA)."
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
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Tsindrio ny bokotra mampidina ho eo amin'ny pejy ambany, eo amin'ny klavie, raha te hahita ny ambin'ny fifanekena ianao."
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
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Mombamomba Azy"
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
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Fampidirana"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Mahandrasa kely, azafady, mandra-pampiditra ny $(^NameDA)."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Vita ny Fampidirana"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Vita soa aman-tsara ny fampidirana."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Installation Aborted"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "Tsy vita hatramin'ny farany ny fampidirana."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Fanesorana"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Mahandrasa kely, azafady, mandra-panaisotra ny $(^NameDA)."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Vita ny Fanesorana"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "Vita soa aman-tsara ny fanesorana."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Notapahina ny Fanesorana"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "Tsy vita hatramin'ny farany ny fanesorana."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Eo Am-pamitana ny Fampidirana ny $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "Tafiditra ato amin'ny ordinateranao ny $(^NameDA).$\r$\n$\r$\nTsindrio ny Vita mba hanakatonana ny Fampidirana."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Tsy maintsy velomina indray ny ordinateranao vao ho vita tanteraka ny fampidirana ny $(^NameDA). Tianao hatao izao ve izany?"
!endif

!ifdef MUI_UNFINISHPAGE
  #FIXME:MUI_UNTEXT_FINISH_INFO_TITLE 
  #FIXME:MUI_UNTEXT_FINISH_INFO_TEXT 
  #FIXME:MUI_UNTEXT_FINISH_INFO_REBOOT 
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Hamelona izao"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Tiako haverina velomina aoriana kely"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Handefa ny $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "H&ampiseho ny VakioAho"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Vita"  
!endif

!ifdef MUI_STARTMENUPAGE
  #FIXME:MUI_TEXT_STARTMENU_TITLE 
  #FIXME:MUI_TEXT_STARTMENU_SUBTITLE 
  #FIXME:MUI_INNERTEXT_STARTMENU_TOP 
  #FIXME: MUI_INNERTEXT_STARTMENU_CHECKBOX 
!endif

!ifdef MUI_UNCONFIRMPAGE
  #FIXME:MUI_UNTEXT_CONFIRM_TITLE 
  #FIXME:MUI_UNTEXT_CONFIRM_SUBTITLE 
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "Tena te hiala ato amin'ny Fampidirana ny $(^Name) ve ianao?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Tena te hiala ato amin'ny Fanesorana ny $(^Name) ve ianao?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_TITLE 
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_SUBTITLE 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_TOP 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER 
!endif
