;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Macedonian (1071)
;By Sasko Zdravkin [vardarce@mail.com]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "MACEDONIAN"

  !define MUI_LANGNAME "Macedonian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Добро Дојдовте на $(^Name) инсталацијата"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ќе се инсталира $(^Name) на вашиот компјутер.\r\n\r\nПрепорачано е да ги затворите сите апликации пред да инсталирате. Ова ќе му дозволи на Инсталациониот Визард да ги надокнади некои системски датотеки без рестартирање на компјутерот.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Лиценцен Договор"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Прочитајте ги лиценцните услови пред да го инсталирате $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Притиснeте Page Down да го видите остатокот од договорот."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ако ги прифаќате условите на договорот, притиснeте Да за да продолжите. Мора да го прифатите Лиценцниот Договор за да продолжите со инсталирањето на $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако ги прифаќате условите на договорот, чекирајте го check box-от подоле. Мора да го прифатите Лиценцниот Договор за да продолжите со инсталирањето на $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако ги прифаќате условите на договорот, одберете ја првата опција подоле. Мора да го прифатите Лиценцниот Договор за да продолжите со инсталирањето на $(^Name). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Одберете Компоненти"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Одберете што сакате од $(^Name) да се инсталира."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Објаснение"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Однесете го глувчето до одредена компонента за да го видите нејзиното објаснение."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Одберете го Инсталациониот Директориум"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Одберете директориум каде што ќе се инсталира $(^Name)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Инсталира"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Ве молиме почекајте додека $(^Name) се инсталира."
  
  !define MUI_TEXT_FINISH_TITLE "Инсталацијата е завршена"
  !define MUI_TEXT_FINISH_SUBTITLE "Инсталирањето заврши успешно."

  !define MUI_TEXT_ABORT_TITLE "Инсталацијата е откажана" 
  !define MUI_TEXT_ABORT_SUBTITLE "Инсталирањето не заврши успешно." 

  !define MUI_BUTTONTEXT_FINISH "&Крај"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завршува Инсталациониот Визард на $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) е инсталиран на компјутерот.\r\n\r\nПритиснeте Крај за да го затворите Инсталациониот Визард."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Компјутерот мора да рестартира за да се заврши инсталирањето на $(^Name). Дали сакате да се рестартира сега?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Рестартирај сега"
  !define MUI_TEXT_FINISH_REBOOTLATER "Сакам јас да го рестартирам подоцна"
  !define MUI_TEXT_FINISH_RUN "Стартувај го $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Отвори ја Readme датотеката"

  !define MUI_TEXT_STARTMENU_TITLE "Одберете Директориум за Старт Менито"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Одберете Старт Мени директориум за скратеницата на прогамот."
  !define MUI_INNERTEXT_STARTMENU_TOP "Одберете Старт Мени директориум во кој што сакате да се креираат скратениците на програмата. Исто така може да пишете име за нов директориум."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не креирај скратеници"
  
  !define MUI_TEXT_ABORTWARNING "Дали сте сигурни дека сакате да го откажете инсталирањето на $(^Name)?"

  
  !define MUI_UNTEXT_CONFIRM_TITLE "Деинсталирај го $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Одстрани го $(^Name) од компјутерот."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталира"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Ве молиме почекајте додека $(^Name) се деинсталира."
    
  !define MUI_UNTEXT_FINISH_TITLE "Деинсталирањето заврши"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Деинсталирањето беше успешно."

  !define MUI_UNTEXT_ABORT_TITLE "Деинсталирањето е откажано" 
  !define MUI_UNTEXT_ABORT_SUBTITLE "Деинсталирањето не заврши успешно."
  
!insertmacro MUI_LANGUAGEFILE_END