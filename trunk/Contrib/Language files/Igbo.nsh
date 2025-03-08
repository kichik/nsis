;Language: Igbo (1136)

!insertmacro LANGFILE "Igbo" = "Igbo" =

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Anyị na-anabata gị ná Ntinye $(^NameDA)"
  #FIXME:MUI_TEXT_WELCOME_INFO_TEXT 
!endif

!ifdef MUI_UNWELCOMEPAGE
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TITLE 
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TEXT 
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Nkwekọrịta Akwụkwọ Ikike"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Biko gụgharịa ihe e dere ná nkwekọrịta akwụkwọ ikike tupu i tinyewe $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Ọ bụrụ na ị nabatara ihe e dere ná nkwekọrịta ahụ, pịa M Kwere iji gaa n'ihu. Ị ga-anabatarịrị nkwekọrịta ahụ iji tinye $(^NameDA)."
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
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Pịa Page Down iji hụ ihe ndị ọzọ dị ná nkwekọrịta a."
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
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Nkọwa"
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
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "O Tinyewela Ya"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Biko chere ka a na-etinyere gị $(^NameDA)."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "O Tinyechaala Ya"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Ntinye gara nke ọ̣ma."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Installation Aborted"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "Ntinye agaghị nke ọma."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "O Wepụwala Ya"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Biko chere ka a na-ewepụrụ gị $(^NameDA)."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "O Wepụchaala Ya"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "O wepụchara ya nke ọma."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "O Wepụlighị Ya"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "O wepụchaghị ya nke ọma."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Itinyecha $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "E tinyela $(^NameDA) na kọmputa gị.$\r$\n$\r$\nPịa Mechaa iji mechie Ntinye."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Ị ga-amaliteghachirịrị kọmputa gị iji tinyechaa $(^NameDA). Ị̀ chọrọ ịmaliteghachi ya ugbu a?"
!endif

!ifdef MUI_UNFINISHPAGE
  #FIXME:MUI_UNTEXT_FINISH_INFO_TITLE 
  #FIXME:MUI_UNTEXT_FINISH_INFO_TEXT 
  #FIXME:MUI_UNTEXT_FINISH_INFO_REBOOT 
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Maliteghachi ugbu a"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "M chọrọ iji aka m maliteghachi ya ma e mechaa"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Malite $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&Gosi Gụọ"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&O mechaala"  
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
  ${LangFileString} MUI_TEXT_ABORTWARNING "Ì ji n'aka ka ị chọghị ka Ntinye $(^Name) ruo n'isi?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Ì ji n'aka na ị chọrọ ka Mwepụ $(^Name) kwụsị?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_TITLE 
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_SUBTITLE 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_TOP 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER 
!endif
