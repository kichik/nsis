;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: Farsi (1065)
;By FzerorubigD

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "FARSI"

  !define MUI_LANGNAME "Ё«—”н" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "  »е »—д«ге д’»$(^Name) ќж‘ ¬гѕнѕ  "
  !define MUI_TEXT_WELCOME_INFO_TEXT "«нд »—д«ге ‘г« —« »—«н д’»  $(^Name) —«едг«нн гнШдѕ.\r\n\r\nБн‘де«ѕ гнШднг Шбне »—д«ге е«н ѕнР— —« ё»б «“ «ѕ«ге д’» »»дѕнѕ. «нд Ш«— »е »—д«ге д’» «ћ«“е гнѕеѕ Ше »ѕжд дн«“ »е —«е «дѕ«“н гћѕѕ Ш«гБнж — Ш«— ќжѕ —« «дћ«г ѕеѕ.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE " ж«Ёёд«ге д’»"  
  !define MUI_TEXT_LICENSE_SUBTITLE "бЎЁ« ё»б «“ д’»  $(^Name) Шбне »дѕе«н  ж«Ёёд«ге —« г—ж— Шднѕ."
  !define MUI_INNERTEXT_LICENSE_TOP "«“ ѕШге е«н Page Up , Down »—«н ѕнѕд Шбне ё”г  е« «” Ё«ѕе Шднѕ."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "«Р— ‘г«  г«г »дѕе«н  ж«Ёёд«ге —« ё»жб ѕ«—нѕ »— —жн ѕШге гж«Ёёг ШбнШ Шднѕ. ‘г« »«нѕ  г«г »дѕе« —« »—«н д’»  $(^Name) ё»жб ѕ«‘ е »«‘нѕ."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "«Р— Шбне »дѕе«н  ж«Ёёд«ге —« ё»жб ѕ«—нѕ НШ »«Ш” “н— —« «д ќ«» Шднѕ.‘г« »«нѕ  г«г »дѕе«н  ж«Ёёд«ге —« »—«н д’» $(^Name) ё»жб ѕ«‘ е »«‘нѕ. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "«Р— Шбне »дѕе«н  ж«Ёёд«ге —« ё»жб ѕ«—нѕ , дќ” нд Р“нде —« «д ќ«» Шднѕ.‘г« »«нѕ  г«г »дѕе«н  ж«Ёёд«ге —« »—«н д’» $(^Name) ё»жб ѕ«‘ е »«‘нѕ. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "«д ќ«» «ћ“«"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "«д ќ«» «ндШе Шѕ«г нШ «“ ё«»бн  е«н  $(^Name) —« гнќж«енѕ д’» Шднѕ."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE " ж÷нЌ« "
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "д‘«дР— г«ж” —« »— —жн е— ё”г  »бџ“«днѕ  «  ж÷нЌ г—»жЎ »е ¬д —« г‘«еѕе Шднѕ."
  
  !define MUI_TEXT_DIRECTORY_TITLE "«д ќ«» гШ«д д’»"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "«д ќ«» Бж‘е «н Ше гнќж«енѕ  $(^Name) ѕ— ¬д д’» ‘жѕ."
  
  !define MUI_TEXT_INSTALLING_TITLE "ѕ— Ќ«б д’»"
  !define MUI_TEXT_INSTALLING_SUBTITLE "бЎЁ«  « “г«дн Ше $(^Name) ѕ— Ќ«б д’» «”  ’»— Шднѕ."
  
  !define MUI_TEXT_FINISH_TITLE "д’» Б«н«д н«Ё "
  !define MUI_TEXT_FINISH_SUBTITLE "»—д«ге д’» $(^Name) »« гжЁён  Б«н«д н«Ё ."
  
  !define MUI_TEXT_ABORT_TITLE "»—д«ге д’» бџж ‘ѕ"
  !define MUI_TEXT_ABORT_SUBTITLE "»—д«ге д’» $(^Name) »« гжЁён  »е Б«н«д д—”нѕ."
  
  !define MUI_BUTTONTEXT_FINISH "&Б«н«д"
  !define MUI_TEXT_FINISH_INFO_TITLE "»—д«ге д’» $(^Name) Б«н«д н«Ё "
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) »— —жн Ш«гБнж — ‘г« д’» ‘ѕ.\r\n\r\n»—«н ќ—жћ »— —жн ѕШге Б«н«д ШбнШ Шднѕ."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Ш«гБнж — ‘г« »«нѕ »—«н Б«н«д д’»  $(^Name) ѕж»«—е —«е «дѕ«“н ‘жѕ. ¬н гнќж«енѕ «б«д «нд Ш«— —« «дћ«г ѕенѕњ"
  !define MUI_TEXT_FINISH_REBOOTNOW "Ш«гБнж — «б«д ѕж»«—е —«е «дѕ«“н ‘жѕ."
  !define MUI_TEXT_FINISH_REBOOTLATER "ќжѕг »Џѕ« «нд Ш«— —« «дћ«г гнѕег."
  !define MUI_TEXT_FINISH_RUN "&«ћ—«н $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&дг«н‘ Ё«нб  ж÷нЌ« "
  
  !define MUI_TEXT_STARTMENU_TITLE "«д ќ«» Бж‘е ѕ— гджн »—д«ге е«"
  !define MUI_TEXT_STARTMENU_SUBTITLE "«д ќ«» Бж‘е ѕ— гджн »—д«ге е« »—«н «нћ«ѕ гн«д»— е«н $(^Name) ѕ— ¬д."
  !define MUI_INNERTEXT_STARTMENU_TOP "Бж‘е «н Ше гнќж«енѕ гн«д»— е«н »—д«ге ѕ— ¬д «нћ«ѕ ‘ждѕ —« «д ќ«» Шднѕ. ‘г« гн ж«днѕ нШ д«г —« »—«н «нћ«ѕ нШ Бж‘е ћѕнѕ  «нБ Шднѕ"
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "гн«д»—н «нћ«ѕ д‘жѕ"
  
  !define MUI_TEXT_ABORTWARNING "ќ«—ћ ‘жнѕњ  $(^Name) ¬н« ‘г« гЎг∆днѕ Ше гнќж«енѕ «“ »—д«ге д’»"  
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "»е »—д«ге Ќ–Ё  $(^Name) ќж‘ ¬гѕнѕ."
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "«нд »—д«ге ‘г« —« »—«н Ќ–Ё $(^Name) «“ —жн Ш«гБнж — «д ШгШ гнШдѕ.\r\n\r\nё»б «“ ¬џ«“ »—д«ге Ќ–Ё гЎг∆д ‘жнѕ Ше $(^Name) ѕ— Ќ«б «ћ—« дн” .\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Ќ–Ё $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Ќ–Ё $(^Name) «“ —жн Ш«гБнж — ‘г«."
  
  !define MUI_UNTEXT_LICENSE_TITLE " ж«Ёёд«ге"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "бЎЁ« Шбне »ќ‘е«н  ж«Ёёд«ге —« ё»б «— Ќ–Ё  $(^Name) г—ж— Шднѕ."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "«Р— ‘г«  г«г »дѕе«н  ж«Ёёд«ге —« ё»жб ѕ«—нѕ »— —жн ѕШге гж«Ёёг ШбнШ Шднѕ. ‘г« »«нѕ  г«г »дѕе« —« »—«н Ќ–Ё $(^Name) ё»жб ѕ«‘ е »«‘нѕ"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "«Р— Шбне »дѕе«н  ж«Ёёд«ге —« ё»жб ѕ«—нѕ НШ »«Ш” “н— —« «д ќ«» Шднѕ.‘г« »«нѕ  г«г »дѕе«н  ж«Ёёд«ге —« »—«н  Ќ–Ё$(^Name) ё»жб ѕ«‘ е »«‘нѕ. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "«Р— Шбне »дѕе«н  ж«Ёёд«ге —« ё»жб ѕ«—нѕ , дќ” нд Р“нде —« «д ќ«» Шднѕ.‘г« »«нѕ  г«г »дѕе«н  ж«Ёёд«ге —« »—«н Ќ–Ё $(^Name) ё»жб ѕ«‘ е »«‘нѕ. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "«д ќ«» «ћ“«"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "ё”г е«нн «“ $(^Name) —« Ше гнќж«енѕ Ќ–Ё ‘ждѕ —« «д ќ«» Шднѕ."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "«д ќ«» гШ«д Ќ–Ё"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Бж‘е «н Ше гнќж«енѕ  $(^Name) «“ ¬д Ќ–Ё ‘жѕ —« «д ќ«» Шднѕ."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "ѕ— Ќ«б Ќ–Ё"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "бЎЁ«  «  $(^Name) ѕ— Ќ«б Ќ–Ё ‘ѕд «”  ’»— Шднѕ."
    
  !define MUI_UNTEXT_FINISH_TITLE "Џгбн«  Ќ–Ё Б«н«д н«Ё "
  !define MUI_UNTEXT_FINISH_SUBTITLE "Џгбн«  Ќ–Ё $(^Name) »« гжЁён  Б«н«д н«Ё ."
  
  !define MUI_UNTEXT_ABORT_TITLE "Џгбн«  Ќ–Ё бџж ‘ѕ."
  !define MUI_UNTEXT_ABORT_SUBTITLE "Џгбн«  Ќ–Ё $(^Name) »« гжЁён  Б«н«д дн«Ё ."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Б«н«д »—д«ге Ќ–Ё$(^Name)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) «“ —жн Ш«гБнж — ‘г« Ќ–Ё ‘ѕ..\r\n\r\n»—«н ќ—жћ «“ »—д«ге Ќ–Ё »— —жн ѕШге Б«н«д ШбнШ Шднѕ.."
  
  !define MUI_UNTEXT_ABORTWARNING "ќ«—ћ ‘жнѕњ $(^Name) ¬н« гЎг∆днѕ Ше гнќж«енѕ «“ »—д«ге д’» "  
  
!insertmacro MUI_LANGUAGEFILE_END