;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.6

;Language: Bulgarian (1026)
;Translated by Asparouh Kalyandjiev [acnapyx@sbline.net]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "BULGARIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Bulgarian" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Натиснете 'Напред', за да продължите."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Натиснете 'Инсталиране', за да започнете инсталацията."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_TITLE "Добре дошли в инсталационния магьосник на ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO "Той ще инсталира ${MUI_PRODUCT} на вашия компютър.\r\n\r\nПрепоръчва се да затворите всички други приложения, преди да стартирате инсталацията. Това ще позволи на инсталацията да обнови някои системни файлове, без да рестартира компютъра.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Лицензно споразумение"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Моля прегледайте лицензните условия преди инсталирането на ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Натиснете 'Page Down', за да видите останалата част от споразумението."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ако приемане всички условия от споразумението, Изберете 'Съгласен съм', за да продължите. Трябва да приемете споразумението, за да инсталирате ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Изберете компонентите"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Изберете кои функции на  ${MUI_PRODUCT} желаете да бъдат инсталирани."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Изберете компонентите, които желаете да се инсталират, и прехнете селекцията от тези, които не желаете да бъдат инсталирани."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Преминете с мишката над определен компонент, за да видите описанието му."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Изберете място на инсталацията"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Изберете папката, в която ${MUI_PRODUCT} да се инсталира."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Инсталацията ще копира  ${MUI_PRODUCT} в следната папка.$\r$\n$\r$\nЗа да инсталирате в друга папка, изберете 'Преглед' и посочете друга папка."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Целева папка"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Инсталиране"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Моля изчакайте, докато ${MUI_PRODUCT} се инсталира."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Инсталацията завърши"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Инсталацията завърши успешно."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": Приключи"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Приключи"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO "${MUI_PRODUCT} е инсталиран на вашата система.\r\nНатиснете 'Приключи', за да затворите магьосника."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Компютърът трябва да бъде рестартиран, за да завърши инсталацията на ${MUI_PRODUCT}. Желаете ли да рестартирате сега?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Рестарт сега"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Желая да рестартирам ръчно по-късно"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Стартирай ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Покажи Readme"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": Папка в Start менюто"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Изберете папка в Start менюто"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Изберете папка в Start менюто за препратките към програмата."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Изберете папка в Start менюто, в която желаете да създадете препратки към файловете на програмата. Можете също така да въведете име, за да създадете нова папка."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Не създавай препратки"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Сигурни ли сте, че желаете да се прекрати инсталацията на ${MUI_PRODUCT}?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Деинсталация на ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Премахване на ${MUI_PRODUCT} от вашата система."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Това ще деинсталира ${MUI_PRODUCT} от вашата система."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталиране"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Моля изчакайте, докато ${MUI_PRODUCT} се деинсталира."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Приключи"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Деинсталацията приключи успешно."
  
!insertmacro MUI_LANGUAGEFILE_END