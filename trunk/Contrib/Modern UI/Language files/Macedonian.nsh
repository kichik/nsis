;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.64

;Language: Macedonian (1071)
;By Sasko Zdravkin [vardarce@mail.com]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "MACEDONIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Macedonian" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Притиснeте Напред за да продолжи."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Притиснeте Инсталирај за да се инсталира."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Добро Дојдовте на ${MUI_PRODUCT} инсталацијата"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ќе се инсталира ${MUI_PRODUCT} на вашиот компјутер.\r\n\r\nПрепорачано е да ги затворите сите апликации пред да инсталирате. Ова ќе му дозволи на Инсталациониот Визард да ги надокнади некои системски датотеки без рестартирање на компјутерот.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Лиценцен Договор"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Прочитајте ги лиценцните услови пред да го инсталирате ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Притиснeте Page Down да го видите остатокот од договорот."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ако ги прифаќате условите на договорот, притиснeте Да за да продолжите. Мора да го прифатите Лиценцниот Договор за да продолжите со инсталирањето на ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако ги прифаќате условите на договорот, чекирајте го check box-от подоле. Мора да го прифатите Лиценцниот Договор за да продолжите со инсталирањето на ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако ги прифаќате условите на договорот, одберете ја првата опција подоле. Мора да го прифатите Лиценцниот Договор за да продолжите со инсталирањето на ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Одберете Компоненти"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Одберете што сакате од ${MUI_PRODUCT} да се инсталира."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Чекирајте ги компонентите што сакате да се инсталираат, а дечекирајте ги тие што не сакате да се инсталираат."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Објаснение"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Однесете го глувчето до одредена компонента за да го видите нејзиното објаснение."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Одберете го Инсталациониот Директориум"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Одберете директориум каде што ќе се инсталира ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Инсталациониот Визард ќе го инсталира ${MUI_PRODUCT} во следниов директориум.$\r$\n$\r$\nЗа да инсталирате во друг директориум, притиснeте Пребарувај и одберете друг директориум."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Одреден Директориум"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Инсталира"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Ве молиме почекајте додека ${MUI_PRODUCT} се инсталира."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Инсталацијата е завршена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Инсталирањето заврши успешно."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": Завршува"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Крај"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Завршува Инсталациониот Визард на ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} е инсталиран на компјутерот.\r\n\r\nПритиснeте Крај за да го затворите Инсталациониот Визард."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Компјутерот мора да рестартира за да се заврши инсталирањето на ${MUI_PRODUCT}. Дали сакате да се рестартира сега?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Рестартирај сега"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Сакам јас да го рестартирам подоцна"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Стартувај го ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Отвори ја Readme датотеката"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": Start Menu Директориум"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Одберете Start Menu Директориум"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Одберете Start Menu директориум за скратеницата на прогамот."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Одберете Start Menu директориум во кој што сакате да се креираат скратениците на програмата. Исто така може да пишете име за нов директориум."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Не креирај скратеници"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Дали сте сигурни дека сакате да го откажете инсталирањето на ${MUI_PRODUCT}?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Инсталацијата е откажана" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Инсталирањето не заврши успешно." 

  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Притиснeте Деинсталирај за да почне деинсталирањето."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Деинсталирај го ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Одстрани го ${MUI_PRODUCT} од компјутерот."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Оваа апликација ќе го деинсталира ${MUI_PRODUCT} од компјутерот."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталира"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Ве молиме почекајте додека ${MUI_PRODUCT} се деинсталира."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Деинсталирањето заврши"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Деинсталирањето беше успешно."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Деинсталирањето е откажано" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Деинсталирањето не заврши успешно."
  
!insertmacro MUI_LANGUAGEFILE_END