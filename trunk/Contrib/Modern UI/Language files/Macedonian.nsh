;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Macedonian (1071)
;By Sasko Zdravkin [vardarce@mail.com]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "MACEDONIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Македонски" ;Name of the language in the language itself
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Добро дојдовте во инсталациониот програм на $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Овој програм ќе ве води низ инсталацијата на $(^Name).\r\n\r\nПрепорачано е да ги затворите сите програми пред да инсталирате. Ова ќе дозволи инсталациониот програм да обнови некои системски датотеки без да го рестартира компјутерот.\r\n\r\n$_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Лиценцен Договор"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Ве молиме проверете ги лиценцните услови пред да го инсталирате $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Притиснете 'Page Down' за да го видете останатиот дел од договорот."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ако ги прифаќате условите од договорот, притиснете 'Да' за да продолжите. Мора да го прифатите договорот за да го инсталирате $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако ги прифаќате условите од договорот, чекирајте го check box-от подоле. Мора да го прифатите договорот за го инсталирате $(^Name). $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако ги прифаќате условите од договорот, одберете ја првата опција подоле. Мора да го прифатите договорот за го инсталирате $(^Name). $_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Одберете Компоненти"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Одберете кои работи од $(^Name) сакате да се инсталираат."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Објаснение"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Однесете го курсорот до компонентата за го видете нејзиното објаснение."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Одберете ја локацијата за инсталирање"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Одберете го директориумот каде што сакате да се инсталира $(^Name)."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Инсталира"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Ве молиме почекајте додека $(^Name) се инсталира."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Инсталацијата е завршена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Инсталирањето беше успешно."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Инсталацијата е откажана"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Инсталирањето не беше успешно завршено."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Крај"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Завршува инсталирањето на $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "$(^Name) е инсталиран на вашиот компјутер.\r\n\r\nПритиснете 'Крај' за да го затворите инсталациониот програм."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Вашиот компјутер мора да се рестартира за да заврши инсталацијата на $(^Name). Дали сакате да се рестартира сега?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Рестартирај сега"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Ако сакате да го рестартирате подоцна"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Пок&рени го $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Отвор&и 'Прочитај Ме'"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Одберете директориум за Старт Менито"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Одберете директориум во Старт Менито за креирање скратеница на $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Одберете го директориумот во Старт Менито во кој сакате да се креира скратеница за програмата. Исто така можете да внесете друго име за да се креира нов директориум."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Не креирај скратеница"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Сигурни ли сте дека сакате да се откажете од инсталацијата на $(^Name)?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Деинсталирај го $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Одстранете го $(^Name) од вашиот компјутер."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_TITLE "Лиценцен Договор"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_SUBTITLE "Ве молиме проверете ги лиценцните услови пред да го деинсталирате $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM "Ако ги прифаќате условите од договорот, притиснете 'Да' за да продолжите. Мора да го прифатите договорот за да го деинсталирате $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако ги прифаќате условите од договорот, чекирајте го check box-от подоле. Мора да го прифатите договорот за го деинсталирате $(^Name). $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако ги прифаќате условите од договорот, одберете ја првата опција подоле. Мора да го прифатите договорот за го деинсталирате $(^Name). $_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Одберете Компоненти"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Одберете кои работи од $(^Name) сакате да се деинсталираат."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_TITLE "Одберете ја локацијата за деинсталирање"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_SUBTITLE "Одберете го директориумот од кој сакате да се деинсталира $(^Name)."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталира"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Ве молиме почекајте додека $(^Name) се деинсталира."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Деинсталацијата е завршена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Деинсталирањето беше успешно."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Деинсталацијата е откажана"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Деинсталирањето не беше успешно завршено."
  
!insertmacro MUI_LANGUAGEFILE_END