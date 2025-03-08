;Language: Efịk (1033)

!insertmacro LANGFILE "Efik" = "Efịk" "Efik"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Emi edi $(^NameDA) Nte Ẹtịmde"
  #FIXME:MUI_TEXT_WELCOME_INFO_TEXT
!endif

!ifdef MUI_UNWELCOMEPAGE
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TITLE 
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TEXT 
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Ediomi Unyịme"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Mbọk kot ediomi unỵme mbemiso esịnde $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Edieke enyịmede kpukpru se ẹtịn̄de ke ediomi emi, fịk Ami Mmenyịme man aka iso. Ana enyịme ediomi emi mîdịghe idisịnke $(^NameDA)."
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
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Fịk Ka Isọn̄ man okụt ediomi emi osụhọde."
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
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Editịn̄ nte etiede"
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
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Ke esịn"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Mbọk bet ke ini esịnde $(^NameDA)."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Esịn Okụre"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Nte Ẹtịmde okụre uforo uforo."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Installation Aborted"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "Nte Ẹtịmde ikụreke uforo uforo."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Ke osio"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Mbọk bet ke ini osiode $(^NameDA) efep."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Osio Efep Okụre"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "Osio Efep Okụre Uforo Uforo."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Etre Ndisio Mfep"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "Isioho Ifep Uforo Uforo."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Nte Ẹtịmde $(^NameDA) Ọmọn̄ Okụre"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "Esịn $(^NameDA) ke kọmputa fo.$\r$\n$\r$\nFịk Okụre man emen Nte Ẹtịmde efep."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Ana afiak ọtọn̄ọ kọmputa fo man okụre edisịn $(^NameDA). Ndi omoyom ndifiak ntọn̄o kọmputa?"
!endif

!ifdef MUI_UNFINISHPAGE
  #FIXME:MUI_UNTEXT_FINISH_INFO_TITLE 
  #FIXME:MUI_UNTEXT_FINISH_INFO_TEXT 
  #FIXME:MUI_UNTEXT_FINISH_INFO_REBOOT 
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Fiak tọn̄ọ idahaemi"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Nyom ndifiak ntọn̄ọ ke idemmi ama ekem"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Kụbọde $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&Wụt Kot-emi"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Okụre"  
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
  ${LangFileString} MUI_TEXT_ABORTWARNING "Ndi emenen̄ede oyom ndiwọrọ ke Nte Ẹtịmde $(^Name)?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Ndi emenen̄ede oyom ndiwọrọ ke Edisio $(^Name) Mfep?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_TITLE 
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_SUBTITLE "
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_TOP 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER 
!endif
