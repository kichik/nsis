;Language: Cibemba (1033)

!insertmacro LANGFILE "Cibemba" = "Cibemba" =

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Apa Kutendekela $(^NameDA) Ukukopolola"
  #FIXME:MUI_TEXT_WELCOME_INFO_TEXT
!endif

!ifdef MUI_UNWELCOMEPAGE
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TITLE
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TEXT
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Ukusuminishanya"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Mukwai belengeni ifili mu kusuminishanya ilyo mushilatendeka ukukopolola $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Nga mwasumina ifili mu kusuminishanya, tinikeni pali Nasumina pa kuti mutwalilile. Mufwile ukusumina ifili mu kusuminishanya pa kuti mukopolole $(^NameDA)."
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
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Kabiyeni pe samba pa kuti mumone fimbi ifili mu kusuminishanya."
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
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Ubulondoloshi"
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
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Ilekopolola"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Mukwai loleleni ilyo $(^NameDA) ilekopolola."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Yapwa Ukukopolola"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Yakopolola."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Yaleka Ukukopolola"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "Taikopolwele."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Ilefuuta"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Mukwai loleleni ilyo $(^NameDA) ilefuuta."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Yapwa Ukufuuta"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "Yafuuta."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Yaleka Ukufuuta"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "Taifuutike."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Ilepwisha $(^NameDA) Ukukopolola"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) Yakopolwela pa kompyuta yenu.$\r$\n$\r$\nTinikeni pa Ukwisala pa kuti mupwishe."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Mufwile ukushimya kompyuta no kuyasha na kabili pa kuti mupwishe ukukopolola $(^NameDA). Bushe mulefwaya ukucita ifi nomba?"
!endif

!ifdef MUI_UNFINISHPAGE
  #FIXME:MUI_UNTEXT_FINISH_INFO_TITLE
  #FIXME:MUI_UNTEXT_FINISH_INFO_TEXT
  #FIXME:MUI_UNTEXT_FINISH_INFO_REBOOT 
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Ndefwaya ukushimya no kwasha ili line"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Nalacita ifi limbi"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Tampeni $(^NameDA)"
  #FIXME:MUI_TEXT_FINISH_SHOWREADME
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Capwa"  
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
  ${LangFileString} MUI_TEXT_ABORTWARNING "Bushe ca cine mulefwaya ukuleka $(^Name) Ukukopolola?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Bushe ca cine mulefwaya ukuleka $(^Name) Ukufuuta?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_TITLE 
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_SUBTITLE 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_TOP 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER 
!endif
