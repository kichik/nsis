;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Irish (2108)
;By Kevin P. Scannell < scannell at slu dot edu >

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Irish"

  !define MUI_LANGNAME "Irish" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Fáilte go dtí Draoi Suiteála $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Déanfaidh an draoi seo treorú duit tríd an suiteáil de $(^NameDA).\r\n\r\nMoltar duit gach feidhmchlár eile a dhúnadh sula dtosaíonn tú an Suiteálaí. Cinnteoidh sé seo gur féidir na comhaid oiriúnacha a nuashonrú gan do ríomhaire a atosú.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Comhaontú um Cheadúnas"
  !define MUI_TEXT_LICENSE_SUBTITLE "Déan iniúchadh ar choinníollacha an cheadúnais sula suiteálann tú $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Brúigh $\"Page Down$\" chun an chuid eile den cheadúnas a léamh."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Má ghlacann tú le coinníollacha an chomhaontaithe, cliceáil $\"Glacaim Leis$\" chun leanúint ar aghaidh. Caithfidh tú glacadh leis an gcomhaontú chun $(^NameDA) a shuiteáil."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Má ghlacann tú le coinníollacha an chomhaontaithe, cliceáil an ticbhosca thíos. Caithfidh tú glacadh leis an gcomhaontú chun $(^NameDA) a shuiteáil. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Má ghlacann tú le coinníollacha an chomhaontaithe, roghnaigh an chéad rogha thíos. Caithfidh tú glacadh leis an gcomhaontú chun $(^NameDA) a dhíshuiteáil. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Roghnaigh Comhpháirteanna"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Roghnaigh na gnéithe $(^NameDA) ba mhaith leat suiteáil."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Cur Síos"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Cuir do luch os cionn comhpháirte chun cur síos a fheiceáil."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Roghnaigh comhpháirt chun cur síos a fheiceáil."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Roghnaigh Suíomh na Suiteála"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Roghnaigh an fillteán inar mian leat $(^NameDA) a shuiteáil."
  
  !define MUI_TEXT_INSTALLING_TITLE "Á Shuiteáil"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Fan go fóill; $(^NameDA) á shuiteáil."
  
  !define MUI_TEXT_FINISH_TITLE "Suiteáil Críochnaithe"
  !define MUI_TEXT_FINISH_SUBTITLE "D'éirigh leis an tsuiteáil."
  
  !define MUI_TEXT_ABORT_TITLE "Suiteáil Tobscortha"
  !define MUI_TEXT_ABORT_SUBTITLE "Níor éirigh leis an tsuiteáil."
  
  !define MUI_BUTTONTEXT_FINISH "&Críochnaigh"
  
  !define MUI_TEXT_FINISH_INFO_TITLE "Draoi Suiteála $(^NameDA) á Chríochnú"
  !define MUI_TEXT_FINISH_INFO_TEXT "Suiteáladh $(^NameDA) ar do ríomhaire.\r\n\r\nCliceáil $\"Críochnaigh$\" chun an draoi seo a dhúnadh."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Ní mór duit do ríomhaire a atosú chun suiteáil $(^NameDA) a chur i gcrích. Ar mhaith leat atosú anois?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Atosaigh anois"
  !define MUI_TEXT_FINISH_REBOOTLATER "Atosóidh mé de láimh níos déanaí"
  !define MUI_TEXT_FINISH_RUN "&Rith $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Tai&speáin comhad README"
  
  !define MUI_TEXT_STARTMENU_TITLE "Roghnaigh Fillteán sa Roghchlár Tosaigh"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Roghnaigh fillteán sa Roghchlár Tosaigh a gcuirfear aicearraí $(^NameDA) ann."
  !define MUI_INNERTEXT_STARTMENU_TOP "Roghnaigh an fillteán sa Roghchlár Tosaigh inar mian leat aicearraí an chláir a chruthú. Is féidir freisin fillteán nua a chruthú trí ainm nua a iontráil."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ná cruthaigh aicearraí"
  
  !define MUI_TEXT_ABORTWARNING "An bhfuil tú cinnte gur mian leat Suiteálaí $(^Name) a scor?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Fáilte go dtí Draoi Díshuiteála $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Déanfaidh an draoi seo treorú duit tríd an díshuiteáil de $(^NameDA).\r\n\r\nBí cinnte nach bhfuil $(^NameDA) ag rith sula dtosaíonn tú an díshuiteáil.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Díshuiteáil $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Bain $(^NameDA) ó do ríomhaire."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Comhaontú um Cheadúnas"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Déan iniúchadh ar choinníollacha an cheadúnais sula ndíshuiteálann tú $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Má ghlacann tú le coinníollacha an chomhaontaithe, cliceáil $\"Glacaim Leis$\" chun leanúint ar aghaidh. Caithfidh tú glacadh leis an gcomhaontú chun $(^NameDA) a dhíshuiteáil."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Má ghlacann tú le coinníollacha an chomhaontaithe, cliceáil an ticbhosca thíos. Caithfidh tú glacadh leis an gcomhaontú chun $(^NameDA) a dhíshuiteáil. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Má ghlacann tú le coinníollacha an chomhaontaithe, roghnaigh an chéad rogha thíos. Caithfidh tú glacadh leis an gcomhaontú chun $(^NameDA) a dhíshuiteáil. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Roghnaigh Comhpháirteanna"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Roghnaigh na gnéithe $(^NameDA) ba mhaith leat díshuiteáil."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Roghnaigh Suíomh na Díshuiteála"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Roghnaigh an fillteán ar mian leat $(^NameDA) a dhíshuiteáil as."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Á Dhíshuiteáil"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Fan go fóill; $(^NameDA) á dhíshuiteáil."
  
  !define MUI_UNTEXT_FINISH_TITLE "Díshuiteáil Críochnaithe"
  !define MUI_UNTEXT_FINISH_SUBTITLE "D'éirigh leis an díshuiteáil."
  
  !define MUI_UNTEXT_ABORT_TITLE "Díshuiteáil Tobscortha"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Níor éirigh leis an díshuiteáil."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Draoi Díshuiteála $(^NameDA) á Chríochnú"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Díshuiteáladh $(^NameDA) ó do ríomhaire.\r\n\r\nCliceáil $\"Críochnaigh$\" chun an draoi seo a dhúnadh."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Ní mór duit do ríomhaire a atosú chun díshuiteáil $(^NameDA) a chur i gcrích. Ar mhaith leat atosú anois?"
  
  !define MUI_UNTEXT_ABORTWARNING "An bhfuil tú cinnte gur mian leat Díshuiteálaí $(^Name) a scor?"

!insertmacro MUI_LANGUAGEFILE_END
