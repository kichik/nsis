;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Farsi (Persian) (1065)
;By Masoud Alinaqian < masoud at m2ix dot com > and FzerorubigD Forud_A2002@Hotmail.Com

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Farsi"

  !define MUI_LANGNAME "Farsi (Persian)" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "»е »—д«ге д’» $(^NameDA) ќж‘ ¬гѕнѕ."
  !define MUI_TEXT_WELCOME_INFO_TEXT "«нд »—д«ге ‘г« —« ѕ— д’» $(^NameDA) —«едг«нн ќж«еѕ Ш—ѕ.\r\n\r\nБн‘де«ѕ гн‘е ё»б «“ ¬џ«“ д’»  г«г »—д«ге е« —« »»дѕнѕ° «нд Ш«— ШгШ ќж«еѕ Ш—ѕ Ё«нбе«н Ќ”«” »ѕжд дн«“ »е ѕж»«—е »«б« ¬гѕд ѕ” Р«е »еЭ—ж“ ‘ждѕ.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE " ж«Ёё д«ге"  
  !define MUI_TEXT_LICENSE_SUBTITLE "бЎЁ« Бн‘ «“ д’» $(^NameDA)‘—«нЎ  ж«ЁёЭд«ге —« гЎ«бЏе Шднѕ."
  !define MUI_INNERTEXT_LICENSE_TOP "Шбнѕ Page Down —« »—«н г‘«еѕе «ѕ«ге  ж«ЁёЭд«ге Ё‘«— ѕенѕ."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "«Р— »« ‘—«нЎ   ж«ЁёЭд«ге гж«Ёёнѕ ё»жб ѕ«—г —« «д ќ«» Шднѕ° »—«н Бн‘Э»—ѕ д’»$(^NameDA) д«Н«—нѕ гж«Ёё »«‘нѕ."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "«Р— »« ‘—«нЎ   ж«ЁёЭд«ге гж«Ёёнѕ ћЏ»е “н— —«  нШ »“днѕ°  »—«н Бн‘Э»—ѕ д’»$(^NameDA) д«Н«—нѕ гж«Ёё »«‘нѕ.$_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "«Р— »« ‘—«нЎ   ж«ЁёЭд«ге гж«Ёёнѕ Р“нде дќ”  «“ Ёе—”  “н— —« «д ќ«» Шднѕ° »—«н Бн‘Э»—ѕ д’»$(^NameDA) д«Н«—нѕ гж«Ёё »«‘нѕ. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Р“нд‘ «ћ“«Ѕ"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "«д ќ«» Шднѕ Не жнОРнЭе«нн «“ $(^NameDA) г«нбнѕ Ше д’» ‘жѕ."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE " ж÷нЌ"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "»—«н г‘«еѕе  ж÷нЌ«  mouse —« —жн нШн «“ «ћ“«Ѕ »»—нѕ."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Р“нд‘ гЌб д’»"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "‘«ќеЭ«н —« »—«н д’» $(^NameDA) «д ќ«» Шднѕ"
  
  !define MUI_TEXT_INSTALLING_TITLE "ѕ—Ќ«б д’»"
  !define MUI_TEXT_INSTALLING_SUBTITLE "гд ў— ‘жнѕ  « $(^NameDA) д’» ‘жѕ."
  
  !define MUI_TEXT_FINISH_TITLE "Ё—¬ндѕ д’» Б«н«д н«Ё ."
  !define MUI_TEXT_FINISH_SUBTITLE "»—д«ге »« гжЁён  д’» ‘ѕ."
  
  !define MUI_TEXT_ABORT_TITLE "д’» г жёЁ ‘ѕ."
  !define MUI_TEXT_ABORT_SUBTITLE "д’» »« гжЁён  Б«н«д дн«Ё ."
  
  !define MUI_BUTTONTEXT_FINISH "&Б«н«д"
  !define MUI_TEXT_FINISH_INFO_TITLE "Б«н«д ѕ«ѕд »е д’» $(^NameDA) ."
  !define MUI_TEXT_FINISH_INFO_TEXT "»—д«ге $(^NameDA) —жн ѕ” Р«е ‘г« д’» ‘ѕ.\r\n\r\n»—«н »” д «нд »—д«ге Б«н«д —« «д ќ«» Шднѕ."
  !define MUI_TEXT_FINISH_INFO_REBOOT "»—«н  Шгнб Ё—¬ндѕ д’» »—д«ге $(^NameDA) ѕ” Р«е ‘г« »«нѕ ѕж»«—е »«б« »н«нѕ° гнќж«енѕ ег «Шджд «нд Ш«— «дћ«г ‘жѕњ"
  !define MUI_TEXT_FINISH_REBOOTNOW "»«б«¬ж—ѕд ѕж»«—е"
  !define MUI_TEXT_FINISH_REBOOTLATER "ќжѕг ѕ— ¬ндѕе «нд Ш«— —« гнШдг"
  !define MUI_TEXT_FINISH_RUN "&«ћ—«н »—д«ге $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&дг«н‘ Ё«нб ¬Р«ен ѕедѕе ѕ— гж—ѕ »—д«ге"
  
  !define MUI_TEXT_STARTMENU_TITLE "‘«ќеЭ«н «“ Ёе—”  Start —« «д ќ«» Шднѕ"
  !define MUI_TEXT_STARTMENU_SUBTITLE "‘«ќеЭ«н —« »—«н ё—«—Р—Ё д гн«дЭ»— е«н »—д«ге $(^NameDA) ѕ— Ёе—”  Start «д ќ«» Шднѕ."
  !define MUI_INNERTEXT_STARTMENU_TOP "‘«ќеЭ«н —« Ше г«нбнѕ гн«дЭ»—е« ѕ— ¬д ”«ќ е ‘жѕ «д ќ«» Шднѕ° егНднд гн ж«днѕ д«г ћѕнѕн —« ж«—ѕ Шднѕ"
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "гн«дЭ»—”«“"
  
  !define MUI_TEXT_ABORTWARNING "нёнёд ѕ«—нѕ Ше гнќж«енѕ «“ »—д«ге д’» $(^Name) ќ«—ћ ‘жнѕ њ"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "»е »—д«ге Б«ШЭ”«“н д—гЭ«Ё“«— $(^NameDA) ќж‘ ¬гѕнѕ."
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "«нд »—д«ге ‘г« —« ѕ— Б«ШЭ”«“н $(^NameDA) —«едг«нн ќж«еѕ Ш—ѕ.\r\n\r\nБн‘ «“ ¬џ«“ Б«ШЭ”«“н гЎгнд ‘жнѕ »—д«ге $(^NameDA) «ћ—« дн”  .\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Б«Ш ”«“н $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Ќ–Ё $(^NameDA) «“ ѕ” Р«е ‘г«."
  
  !define MUI_UNTEXT_LICENSE_TITLE " ж«ЁёЭд«ге"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Бн‘ «“ Б«ШЭ”«“н$(^NameDA)°  ж«Ёё д«ге —« »ќж«днѕ. "
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "«Р— »« ‘—«нЎ   ж«ЁёЭд«ге гж«Ёёнѕ ё»жб ѕ«—г —« «д ќ«» Шднѕ° »—«н Б«ШЭ”«“н$(^NameDA) д«Н«—нѕ гж«Ёё »«‘нѕ."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "«Р— »« ‘—«нЎ   ж«ЁёЭд«ге гж«Ёёнѕ ћЏ»е “н— —«  нШ »“днѕ°  »—«н Б«ШЭ”«“н$(^NameDA) д«Н«—нѕ гж«Ёё »«‘нѕ.$_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "«Р— »« ‘—«нЎ   ж«ЁёЭд«ге гж«Ёёнѕ Р“нде дќ”  «“ Ёе—”  “н— —« «д ќ«» Шднѕ° »—«н Б«ШЭ”«“н$(^NameDA) д«Н«—нѕ гж«Ёё »«‘нѕ. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Р“нд‘ «ћ“«Ѕ"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "д ќ«» Шднѕ Не жнОРнЭе«нн «“ $(^NameDA) г«нбнѕ Ше Б«ШЭ”«“н ‘жѕ."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Р“нд‘ гЌб Б«Ш ”«“н"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "гЌбн —« Ше г«нбнѕ »—д«ге $(^NameDA) «“ ¬д Б«ШЭ”«“н ‘жѕ «д ќ«» Шднѕ."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "ѕ— Ќ«б Б«ШЭ”«“н"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "бЎЁ« гд ў— ‘жнѕ  « »—д«ге $(^NameDA) Б«ШЭ”«“н ‘жѕ."
    
  !define MUI_UNTEXT_FINISH_TITLE "Б«ШЭ”«“н Б«н«д н«Ё "
  !define MUI_UNTEXT_FINISH_SUBTITLE "Б«ШЭ”«“н »« гжЁён  Б«н«д н«Ё ."
  
  !define MUI_UNTEXT_ABORT_TITLE " жёЁ Б«ШЭ”«“н"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Б«ШЭ”«“н »е Ўж— Ш«гб «дћ«г д‘ѕ."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Б«н«д »—д«ге Б«ШЭ”«“н д—г «Ё“«—$(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "д—г «Ё“«— $(^NameDA)Э«“ Ш«гБнж — ‘г« Б«Ш ‘ѕ.\r\n\r\nБ«н«д —« »—«н  г«г ‘ѕд »—д«ге «д ќ«» Шднѕ."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "»—«н  Шгнб Ё—¬ндѕ Б«ШЭ”«“н $(^NameDA) ѕ” Р«е ‘г« »«нѕ ѕж»«—е »«б« »н«нѕ° гнќж«енѕ ег «Шджд «нд Ш«— «дћ«г ‘жѕњ"
  
  !define MUI_UNTEXT_ABORTWARNING "нёнёд ѕ«—нѕ Ше гнќж«енѕ «“ »—д«ге Б«ШЭ”«“н $(^Name) ќ«—ћ ‘жнѕ њ"
  
!insertmacro MUI_LANGUAGEFILE_END