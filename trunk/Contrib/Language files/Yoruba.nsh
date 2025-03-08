;Language: Yorùbá (1033)

!insertmacro LANGFILE "Yoruba" = "Yorùbá" "Yoruba"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Èyí ni Ètò Ìṣiṣẹ́ $(^NameDA)"
  #FIXME:MUI_TEXT_WELCOME_INFO_TEXT 
!endif

!ifdef MUI_UNWELCOMEPAGE
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TITLE 
  #FIXME:MUI_UNTEXT_WELCOME_INFO_TEXT "
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Àdéhùn Ìfúnniláṣẹ"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Jọ̀wọ́ ka àdéhùn ìfúnniláṣẹ kó o tó fi $(^NameDA) sí i."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "tó o bá fara mọ́ ohun tá a sọ nínú àdéhùn yìí, tẹ Mo Gbà kó lè máa bá a lọ. O gbọ́dọ̀ fara mọ́ àdéhùn yìí kó o tó fi $(^NameDA) sí i."
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
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Tẹ bọ́tìnì Page Down láti lè rí ìyókù àdéhùn yìí."
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
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Àlàyé nípa rẹ̀"
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
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Ó ti ń fi sí i"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Jọ̀wọ́ ní sùúrù bó ṣe ń fi $(^NameDA) sí i."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Ó ti fi sí i tán"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Ó ti parí lórí ètò ìṣiṣẹ́."
  #FIXME:MUI_TEXT_ABORT_TITLE 
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "Kò lè parí ètò ìṣiṣẹ́."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Ó ti ń yọ ọ́"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Jọ̀wọ́ ní sùúrù bó ṣe ń yọ $(^NameDA)."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Ó Ti Yọ Ọ́ Tán"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "Ó ti parí yíyọ ọ́."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Ó Ti Ṣíwọ́ Yíyọ Ọ́"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "Kò parí yíyọ ọ́."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Ó ń parí Ètò Ìṣiṣẹ́ $(^NameDA) lọ"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT Ó ti fi "$(^NameDA) sórí kọ̀ǹpútà rẹ.$\r$\n$\r$\nTẹ Ìparí kó lè pa Ètò Ìṣiṣẹ́."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "O ní láti tún kọ̀ǹpútà rẹ̣ tàn kó tó lè parí iṣẹ́ fífi $(^NameDA) sí i. Ṣe wàá fẹ́ tún kọ̀ǹpútà rẹ tàn báyìí?"
!endif

!ifdef MUI_UNFINISHPAGE
  #FIXME:MUI_UNTEXT_FINISH_INFO_TITLE 
  #FIXME:MUI_UNTEXT_FINISH_INFO_TEXT 
  #FIXME:MUI_UNTEXT_FINISH_INFO_REBOOT 
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Tún un tàn wàyí"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Mo fẹ́ fúnra mi tún un tàn tó bá yá"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Mú kí $(^NameDA) bẹ̀rẹ̀ iṣẹ́"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&Gbé Kàmí jáde"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Ìparí"  
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
  ${LangFileString} MUI_TEXT_ABORTWARNING "Ṣé ó dá ọ lójú pé o fẹ́ pa Ètò Ìṣiṣẹ́ $(^Name) tì?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Ṣé ó dá ọ lójú pé o fẹ́ pa Yíyọ $(^Name) Kúrò tì?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_TITLE 
  #FIXME:MULTIUSER_TEXT_INSTALLMODE_SUBTITLE 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_TOP 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS 
  #FIXME:MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER 
!endif
