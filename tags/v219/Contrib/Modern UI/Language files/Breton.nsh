;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Breton (1150)
;By KAD-Korvigelloù An Drouizig

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Breton"

  !define MUI_LANGNAME "Brezhoneg" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Degemer mat e skoazeller staliañ $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Sturiet e viot gant ar skoazeller-mañ a-benn staliañ $(^NameDA).\r\n\r\nGwelloc'h eo serriñ pep arload oberiant er reizhiad a-raok mont pelloc'h gant ar skoazeller-mañ. Evel-se e c'heller nevesaat ar restroù reizhiad hep rankout adloc'hañ hoc'h urzhiataer.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Aotre arverañ"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Bezit aketus en ur lenn pep term eus an aotre a-raok staliañ $(^NameDA), mar plij."
  !define MUI_INNERTEXT_LICENSE_TOP "Pouezit war « Pajenn a-raok » evit lenn ar pajennoù eus an aotre da-heul."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Mar degemerit pep term eus an aotre, klikit war « War-lerc'h ». Ret eo deoc'h degemer an aotre evit staliañ $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Mar degemerit pep term eus an aotre, klikit war al log a-zindan. Ret eo deoc'h degemer an aotre a-benn staliañ $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Mar degemerit pep term eus an aotre, diuzit an dibab kentañ a-zindan. Ret eo deoc'h degemer an aotre a-benn staliañ $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Dibab elfennoù"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Dibabit pe elfenn(où) $(^NameDA) a fell deoc'h staliañ."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Deskrivadenn"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Lakait ho logodenn a-zioc'h an elfenn evit gwelout he deskrivadenn."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Diuzuit an elfenn evit gwelout he deskrivadenn."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Dibabit al lec'hiadur staliañ"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Dibabit ar c'havlec'h ma vo lakaet $(^NameDA) ennañ."
  
  !define MUI_TEXT_INSTALLING_TITLE "O staliañ"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Gortozit mar plij, emañ $(^NameDA) o vezañ staliet."
  
  !define MUI_TEXT_FINISH_TITLE "Echu eo gant ar staliañ"
  !define MUI_TEXT_FINISH_SUBTITLE "Kaset eo bet da benn mat ar staliañ."
  
  !define MUI_TEXT_ABORT_TITLE "Staliañ paouezet"
  !define MUI_TEXT_ABORT_SUBTITLE "N'eo ket bet kaset da benn mat ar staliañ."
  
  !define MUI_BUTTONTEXT_FINISH "&Echuiñ"
  !define MUI_TEXT_FINISH_INFO_TITLE "Oc'h echuiñ staliañ $(^NameDA) gant ar skoazeller"
  !define MUI_TEXT_FINISH_INFO_TEXT "Staliet eo bet $(^NameDA) war hoc'h urzhiataer.\r\n\r\nKlikit war « Echuiñ » evit serriñ ar skoazeller-mañ."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Ret eo hoc'h urzhiataer bezañ adloc'het evit ma vez kaset da benn staliañ $(^NameDA). Ha fellout a ra deoc'h adloc'hañ diouzhtu ?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Adloc'hañ diouzhtu"
  !define MUI_TEXT_FINISH_REBOOTLATER "Fellout a ra din adloc'hañ diwezatoc'h dre zorn"
  !define MUI_TEXT_FINISH_RUN "&Lañsañ $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Diskouez ar restr Malennit"
  
  !define MUI_TEXT_STARTMENU_TITLE "Diskouez kavlec'h al Lañser loc'hañ"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Dibabit ur c'havlec'h Lañser loc'hañ evit berradennoù $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Diuzit ar c'havlec'h Lañser loc'hañ e vo savet ennañ berradennoù ar goulevioù. Gallout a rit ingal reiñ un anv evit sevel ur c'havlec'h nevez."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Chom hep sevel berradennoù"
  
  !define MUI_TEXT_ABORTWARNING "Ha sur oc'h e fell deoc'h kuitaat staliañ $(^Name) ?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Degemer mat er skoazeller distaliañ $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Sturiet e viot gant ar skoazeller-mañ a-benn distaliañ $(^NameDA).\r\n\r\nEn em asurit n'eo ket lañset $(^NameDA) a-raok mont pelloc'h gant an distaliañ.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Distaliañ $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Dilemel $(^NameDA) adalek hoc'h urzhiataer."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Aotre arverañ"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Trugarez da lenn an aotre arverañ a-raok distaliañ $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Mar degemerit pep term eus an aotre, klikit war « A-du on » evit kenderc'hel. Ret eo deoc'h degemer an aotre evit distaliañ $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Mar degemerit pep term eus an aotre, klikit war al log a-zindan. Ret eo deoc'h degemer an aotre evit distaliañ $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Mar degemerit pep term eus an aotre, dizuit an dibab kentañ a-zindan. Ret eo deoc'h degemer an aotre evit distaliañ $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Dibabit elfennoù"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Dibabit pe elfenn(où) $(^NameDA) a fell deoc'h distaliañ."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Dibabit al lec'hiadur distaliañ"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Dibabit ar c'havlec'h e vo dilamet $(^NameDA) dioutañ."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "O tistaliañ"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Gortozit mar plij, emañ $(^NameDA) o vezañ distaliet."
    
  !define MUI_UNTEXT_FINISH_TITLE "Echu eo gant an distaliañ"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Kaset eo bet da benn mat an distaliañ."
  
  !define MUI_UNTEXT_ABORT_TITLE "Distaliañ paouezet"
  !define MUI_UNTEXT_ABORT_SUBTITLE "N'eo ket bet kaset da benn mat an distaliañ."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Oc'h echuiñ distaliañ $(^NameDA) gant ar skoazeller"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Distaliet eo bet $(^NameDA) diouzh hoc'h urzhiataer.\r\n\r\nKlikit war « Echuiñ » evit serriñ ar skoazeller-mañ."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Ret eo hoc'h urzhiataer bezañ adloc'het evit ma vez kaset da benn distaliañ $(^NameDA). Ha fellout a ra deoc'h adloc'hañ diouzhtu ?"
  
  !define MUI_UNTEXT_ABORTWARNING "Ha sur oc'h e fell deoc'h kuitaat distaliañ $(^Name) ?"
  
!insertmacro MUI_LANGUAGEFILE_END
