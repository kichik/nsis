;Language: Vietnamese (1066)
;By NGUYÊŞN Maònh HuÌng <loveleeyoungae@yahoo.com>

!insertmacro LANGFILE "Vietnamese" "Vietnamese"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "ChaÌo mıÌng ğêìn võìi Trõò liì CaÌi ğãòt $(^NameDA)"
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TEXT "TriÌnh trõò liì seŞ hıõìng dâŞn baòn viêòc caÌi ğãòt $(^NameDA).$\r$\n$\r$\nBaòn nên ğoìng tâìt caÒ caìc chıõng triÌnh khaìc trıõìc khi bãìt ğâÌu caÌi ğãòt. ĞiêÌu naÌy coì thêÒ giuìp câòp nhâòt caìc tâòp tin hêò thôìng maÌ không câÌn phaÒi khõÒi ğôòng laòi maìy tiình.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TITLE "ChaÌo mıÌng ğêìn võìi Trõò liì GõŞ boÒ $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TEXT "TriÌnh trõò liì seŞ hıõìng dâŞn baòn viêòc gõŞ boÒ $(^NameDA).$\r$\n$\r$\nTrıõìc khi bãìt ğâÌu gõŞ boÒ, haŞy chãìc chãìn rãÌng $(^NameDA) ğang không chaòy.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "ThoÒa thuâòn Giâìy pheìp"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Vui loÌng xem xeìt caìc ğiêÌu khoaÒn giâìy pheìp trıõìc khi caÌi ğãòt $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Nêìu baòn châìp thuâòn caìc ğiêÌu khoaÒn cuÒa thoÒa thuâòn, haŞy nhâìn “Tôi ğôÌng yì” ğêÒ tiêìp tuòc. Baòn phaÒi châìp thuâòn baÒn thoÒa thuâòn ğêÒ caÌi ğãòt $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Nêìu baòn châìp thuâòn caìc ğiêÌu khoaÒn cuÒa thoÒa thuâòn, haŞy nhâìn ô bên dıõìi. Baòn phaÒi châìp thuâòn baÒn thoÒa thuâòn ğêÒ caÌi ğãòt $(^NameDA). $_CLICK"
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Nêìu baòn châìp thuâòn caìc ğiêÌu khoaÒn cuÒa thoÒa thuâòn, haŞy choòn ô ğâÌu tiên bên dıõìi. Baòn phaÒi châìp thuâòn baÒn thoÒa thuâòn ğêÒ caÌi ğãòt $(^NameDA). $_CLICK"
!endif

!ifdef MUI_UNLICENSEPAGE
  ${LangFileString} MUI_UNTEXT_LICENSE_TITLE "ThoÒa thuâòn Giâìy pheìp"
  ${LangFileString} MUI_UNTEXT_LICENSE_SUBTITLE "Vui loÌng xem xeìt caìc ğiêÌu khoaÒn giâìy pheìp trıõìc khi gõŞ boÒ $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM "Nêìu baòn châìp thuâòn caìc ğiêÌu khoaÒn cuÒa thoÒa thuâòn, haŞy nhâìn “Tôi ğôÌng yì” ğêÒ tiêìp tuòc. Baòn phaÒi châìp thuâòn baÒn thoÒa thuâòn ğêÒ gõŞ boÒ $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Nêìu baòn châìp thuâòn caìc ğiêÌu khoaÒn cuÒa thoÒa thuâòn, haŞy nhâìn ô bên dıõìi. Baòn phaÒi châìp thuâòn baÒn thoÒa thuâòn ğêÒ gõŞ boÒ $(^NameDA). $_CLICK"
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Nêìu baòn châìp thuâòn caìc ğiêÌu khoaÒn cuÒa thoÒa thuâòn, haŞy choòn ô ğâÌu tiên bên dıõìi. Baòn phaÒi châìp thuâòn baÒn thoÒa thuâòn ğêÒ gõŞ boÒ $(^NameDA). $_CLICK"
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Âìn Page Down ğêÒ xem phâÌn coÌn laòi cuÒa thoÒa thuâòn."
!endif

!ifdef MUI_COMPONENTSPAGE
  ${LangFileString} MUI_TEXT_COMPONENTS_TITLE "Choòn thaÌnh phâÌn"
  ${LangFileString} MUI_TEXT_COMPONENTS_SUBTITLE "Choòn caìc tiình nãng cuÒa $(^NameDA) maÌ baòn muôìn caÌi ğãòt."
!endif

!ifdef MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_UNTEXT_COMPONENTS_TITLE "Choòn thaÌnh phâÌn"
  ${LangFileString} MUI_UNTEXT_COMPONENTS_SUBTITLE "Choòn caìc tiình nãng cuÒa $(^NameDA) maÌ baòn muôìn gõŞ boÒ."
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Mô taÒ"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Rê chuôòt lên trên môòt thaÌnh phâÌn ğêÒ thâìy mô taÒ cuÒa noì."
  !else
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Rê chuôòt lên trên môòt thaÌnh phâÌn ğêÒ thâìy mô taÒ cuÒa noì."
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  ${LangFileString} MUI_TEXT_DIRECTORY_TITLE "Choòn thı muòc caÌi ğãòt"
  ${LangFileString} MUI_TEXT_DIRECTORY_SUBTITLE "Choòn thı muòc ğêÒ caÌi ğãòt $(^NameDA)."
!endif

!ifdef MUI_UNDIRECTORYPAGE
  ${LangFileString} MUI_UNTEXT_DIRECTORY_TITLE "Choòn thı muòc gõŞ boÒ"
  ${LangFileString} MUI_UNTEXT_DIRECTORY_SUBTITLE "Choòn thı muòc ğêÒ gõŞ boÒ $(^NameDA)."
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "CaÌi ğãòt"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Vui loÌng ğõòi trong khi $(^NameDA) ğang ğıõòc caÌi ğãòt."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "CaÌi ğãòt hoaÌn tâìt"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Viêòc caÌi ğãòt ğaŞ hoaÌn tâìt thaÌnh công."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "CaÌi ğãòt biò huÒy"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "Viêòc caÌi ğãòt không hoaÌn tâìt thaÌnh công."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "GõŞ boÒ"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vui loÌng ğõòi trong khi $(^NameDA) ğang ğıõòc gõŞ boÒ."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "GõŞ boÒ hoaÌn tâìt"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "Viêòc gõŞ boÒ ğaŞ hoaÌn tâìt thaÌnh công."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "GõŞ boÒ biò huÒy"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "Viêòc gõŞ boÒ không hoaÌn tâìt thaÌnh công."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "HoaÌn tâìt CaÌi ğãòt $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) ğaŞ ğıõòc caÌi ğãòt vaÌo maìy tiình cuÒa baòn.$\r$\n$\r$\nNhâìn “HoaÌn thaÌnh” ğêÒ ğoìng TriÌnh trõò liì."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Maìy tiình cuÒa baòn phaÒi ğıõòc khõÒi ğôòng laòi ğêÒ hoaÌn tâìt viêòc caÌi ğãòt $(^NameDA). Baòn coì muôìn khõÒi ğôòng laòi ngay không?"
!endif

!ifdef MUI_UNFINISHPAGE
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TITLE "HoaÌn tâìt GõŞ boÒ $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) ğaŞ ğıõòc gõŞ boÒ khoÒi maìy tiình cuÒa baòn.$\r$\n$\r$\nNhâìn “HoaÌn thaÌnh” ğêÒ ğoìng TriÌnh trõò liì."
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_REBOOT "Maìy tiình cuÒa baòn phaÒi ğıõòc khõÒi ğôòng laòi ğêÒ hoaÌn tâìt viêòc gõŞ boÒ $(^NameDA). Baòn coì muôìn khõÒi ğôòng laòi ngay không?"
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "KhõÒi ğôòng laòi ngay"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Tôi muôìn khõÒi ğôòng laòi sau"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Chaòy $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "Hiêòn &Readme"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&HoaÌn thaÌnh"  
!endif

!ifdef MUI_STARTMENUPAGE
  ${LangFileString} MUI_TEXT_STARTMENU_TITLE "Choòn thı muòc TriÌnh ğõn Start"
  ${LangFileString} MUI_TEXT_STARTMENU_SUBTITLE "Choòn môòt thı muòc trên TriÌnh ğõn Start ğêÒ taòo lôìi tãìt cho $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_TOP "Choòn thı muòc trên TriÌnh ğõn Start maÌ baòn muôìn taòo lôìi tãìt cho chıõng triÌnh. Baòn cuŞng coì thêÒ nhâòp tên ğêÒ taòo thı muòc mõìi."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_CHECKBOX "Không taòo lôìi tãìt"
!endif

!ifdef MUI_UNCONFIRMPAGE
  ${LangFileString} MUI_UNTEXT_CONFIRM_TITLE "GõŞ boÒ $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_CONFIRM_SUBTITLE "GõŞ boÒ $(^NameDA) khoÒi maìy tiình cuÒa baòn."
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "Baòn coì thâòt sıò muôìn thoaìt triÌnh CaÌi ğãòt $(^Name) không?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Baòn coì thâòt sıò muôìn thoaìt triÌnh GõŞ boÒ $(^Name) không?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  ${LangFileString} MULTIUSER_TEXT_INSTALLMODE_TITLE "Choòn ngıõÌi duÌng"
  ${LangFileString} MULTIUSER_TEXT_INSTALLMODE_SUBTITLE "Choòn ngıõÌi duÌng maÌ baòn muôìn caÌi ğãòt $(^NameDA)."
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_TOP "Choòn giıŞa viêòc caÌi ğãòt $(^NameDA) cho riêng baòn hoãòc cho tâìt caÒ ngıõÌi duÌng cuÒa maìy tiình naÌy. $(^ClickNext)"
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS "CaÌi ğãòt cho bâìt kiÌ ngıõÌi naÌo sıÒ duòng maìy tiình naÌy"
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER "ChiÒ caÌi ğãòt cho riêng tôi"
!endif
