;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Macedonian (1071)
;By Sasko Zdravkin [vardarce@mail.com]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "MACEDONIAN"

  !define MUI_LANGNAME "Македонски" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Добро дојдовте во инсталациониот програм на $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Овој програм ќе ве води низ инсталацијата на $(^Name).\r\n\r\nПрепорачано е да ги затворите сите програми пред да инсталирате. Ова ќе дозволи инсталациониот програм да обнови некои системски датотеки без да го рестартира компјутерот.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Лиценцен Договор"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Ве молиме проверете ги лиценцните услови пред да го инсталирате $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Притиснете 'Page Down' за да го видете останатиот дел од договорот."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ако ги прифаќате условите од договорот, притиснете 'Да' за да продолжите. Мора да го прифатите договорот за да го инсталирате $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако ги прифаќате условите од договорот, чекирајте го check box-от подоле. Мора да го прифатите договорот за го инсталирате $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако ги прифаќате условите од договорот, одберете ја првата опција подоле. Мора да го прифатите договорот за го инсталирате $(^Name). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Одберете Компоненти"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Одберете кои работи од $(^Name) сакате да се инсталираат."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Објаснение"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Однесете го курсорот до компонентата за го видете нејзиното објаснение."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Одберете ја локацијата за инсталирање"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Одберете го директориумот каде што сакате да се инсталира $(^Name)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Инсталира"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Ве молиме почекајте додека $(^Name) се инсталира."
  
  !define MUI_TEXT_FINISH_TITLE "Инсталацијата е завршена"
  !define MUI_TEXT_FINISH_SUBTITLE "Инсталирањето беше успешно."
  
  !define MUI_TEXT_ABORT_TITLE "Инсталацијата е откажана"
  !define MUI_TEXT_ABORT_SUBTITLE "Инсталирањето не беше успешно завршено."
  
  !define MUI_BUTTONTEXT_FINISH "&Крај"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завршува инсталирањето на $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) е инсталиран на вашиот компјутер.\r\n\r\nПритиснете 'Крај' за да го затворите инсталациониот програм."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Вашиот компјутер мора да се рестартира за да заврши инсталацијата на $(^Name). Дали сакате да се рестартира сега?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Рестартирај сега"
  !define MUI_TEXT_FINISH_REBOOTLATER "Ако сакате да го рестартирате подоцна"
  !define MUI_TEXT_FINISH_RUN "Пок&рени го $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Отвор&и 'Прочитај Ме'"
  
  !define MUI_TEXT_STARTMENU_TITLE "Одберете директориум за Старт Менито"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Одберете директориум во Старт Менито за креирање скратеница на $(^Name)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Одберете го директориумот во Старт Менито во кој сакате да се креира скратеница за програмата. Исто така можете да внесете друго име за да се креира нов директориум."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не креирај скратеница"
  
  !define MUI_TEXT_ABORTWARNING "Сигурни ли сте дека сакате да се откажете од инсталацијата на $(^Name)?"  
  
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Деинсталирај го $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Одстранете го $(^Name) од вашиот компјутер."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Лиценцен Договор"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Ве молиме проверете ги лиценцните услови пред да го деинсталирате $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ако ги прифаќате условите од договорот, притиснете 'Да' за да продолжите. Мора да го прифатите договорот за да го деинсталирате $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако ги прифаќате условите од договорот, чекирајте го check box-от подоле. Мора да го прифатите договорот за го деинсталирате $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако ги прифаќате условите од договорот, одберете ја првата опција подоле. Мора да го прифатите договорот за го деинсталирате $(^Name). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Одберете Компоненти"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Одберете кои работи од $(^Name) сакате да се деинсталираат."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Одберете ја локацијата за деинсталирање"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Одберете го директориумот од кој сакате да се деинсталира $(^Name)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталира"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Ве молиме почекајте додека $(^Name) се деинсталира."
    
  !define MUI_UNTEXT_FINISH_TITLE "Деинсталацијата е завршена"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Деинсталирањето беше успешно."
  
  !define MUI_UNTEXT_ABORT_TITLE "Деинсталацијата е откажана"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Деинсталирањето не беше успешно завршено."
  
!insertmacro MUI_LANGUAGEFILE_END