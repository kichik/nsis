;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.63

;Language: Bulgarian (1026)
;Translated by Asparouh Kalyandjiev [acnapyx@sbline.net]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "BULGARIAN"

  !define MUI_LANGNAME "Bulgarian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Добре дошли в инсталационния магьосник на $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Той ще инсталира $(^NameDA) на вашия компютър.\r\n\r\nПрепоръчва се да затворите всички други приложения, преди да стартирате инсталацията. Това ще позволи на инсталацията да обнови някои системни файлове, без да рестартира компютъра.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Лицензно споразумение"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Моля прегледайте лицензните условия преди инсталирането на $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Натиснете 'Page Down', за да видите останалата част от споразумението."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ако приемате всички условия от споразумението, Изберете 'Съгласен', за да продължите. Трябва да приемете споразумението, за да инсталирате $(^NameDA)."
  
  !define MUI_TEXT_COMPONENTS_TITLE "Изберете компонентите"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Изберете кои функции на  $(^NameDA) желаете да бъдат инсталирани."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Преминете с мишката над определен компонент, за да видите описанието му."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Изберете място на инсталацията"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Изберете папката, в която $(^NameDA) да се инсталира."
  
  !define MUI_TEXT_INSTALLING_TITLE "Инсталиране"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Моля изчакайте, докато $(^NameDA) се инсталира."
  
  !define MUI_BUTTONTEXT_FINISH "&Приключи"
  !define MUI_TEXT_FINISH_TITLE "Инсталацията завърши"
  !define MUI_TEXT_FINISH_SUBTITLE "Инсталацията завърши успешно."
  !define MUI_TEXT_FINISH_INFO_TITLE "Завършване на магьосника за инсталация на $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) е инсталиран на вашата система.\r\nНатиснете 'Приключи', за да затворите магьосника."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Компютърът трябва да бъде рестартиран, за да завърши инсталацията на $(^NameDA). Желаете ли да рестартирате сега?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Рестарт сега"
  !define MUI_TEXT_FINISH_REBOOTLATER "Желая да рестартирам ръчно по-късно"
  !define MUI_TEXT_FINISH_RUN "Стартирай $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Покажи Readme"
  
  !define MUI_TEXT_STARTMENU_TITLE "Изберете папка в Start менюто"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Изберете папка в Start менюто за препратките към програмата."
  !define MUI_INNERTEXT_STARTMENU_TOP "Изберете папка в Start менюто, в която желаете да създадете препратки към файловете на програмата. Можете също така да въведете име, за да създадете нова папка."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не създавай препратки"
  
  !define MUI_TEXT_ABORTWARNING "Сигурни ли сте, че желаете да се прекрати инсталацията на $(^Name)?"  
  
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Деинсталация на $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Премахване на $(^NameDA) от вашата система."
   
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталиране"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Моля изчакайте, докато $(^NameDA) се деинсталира."
    
  !define MUI_UNTEXT_FINISH_TITLE "Приключи"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Деинсталацията приключи успешно."
  
!insertmacro MUI_LANGUAGEFILE_END