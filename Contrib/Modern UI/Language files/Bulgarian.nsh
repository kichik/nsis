;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Bulgarian (1026)
;Translated by Asparouh Kalyandjiev [acnapyx@sbline.net]
;Review and update from v1.63 to v1.68 by Plamen Penkov <plamen_mbx@yahoo.com>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Bulgarian"

  !define MUI_LANGNAME "Bulgarian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Добре дошли в инстала-\r\nционния магьосник на $(^NameDA)!"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Той ще инсталира $(^NameDA) на вашия компютър.\r\n\r\nПрепоръчва се да затворите всички други приложения, преди да стартирате инсталацията. Това ще позволи на инсталатора да обнови някои системни файлове, без да рестартира компютъра.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Лицензионно споразумение"
  !define MUI_TEXT_LICENSE_SUBTITLE "Моля прегледайте лицензионните условия преди инсталирането на $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Натиснете 'Page Down', за да видите останалата част от споразумението."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ако приемате условията на споразумението, натиснете 'Съгласен', за да продължите. Трябва да приемете споразумението, за да инсталирате $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако приемате условията на споразумението, отметнете кутийката по-долу. Трябва да приемете споразумението, за да инсталирате $(^NameDA) $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако приемате условията на споразумението, изберете първата опция по-долу. Трябва да приемете споразумението, за да инсталирате $(^NameDA) $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Изберете компонентите"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Изберете кои функции на  $(^NameDA) желаете да бъдат инсталирани."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Преминете с мишката над определен компонент, за да видите описанието му."

  !define MUI_TEXT_DIRECTORY_TITLE "Изберете място на инсталацията"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Изберете папката, в която $(^NameDA) да се инсталира."
  
  !define MUI_TEXT_INSTALLING_TITLE "Инсталиране"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Моля изчакайте, докато $(^NameDA) се инсталира."
 
  !define MUI_TEXT_FINISH_TITLE "Инсталацията завърши."
  !define MUI_TEXT_FINISH_SUBTITLE "Инсталацията завърши успешно."

  !define MUI_TEXT_ABORT_TITLE "Инсталацията прекъсна."
  !define MUI_TEXT_ABORT_SUBTITLE "Инсталацията не завърши успешно."
    
  !define MUI_BUTTONTEXT_FINISH "&Приключи"
  !define MUI_TEXT_FINISH_INFO_TITLE "Затваряне на магьосника за инсталация на $(^NameDA)."
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) е инсталиран на вашата система.\r\nНатиснете 'Приключи', за да затворите магьосника."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Компютърът трябва да бъде рестартиран, за да завърши инсталацията на $(^NameDA). Желаете ли да рестартирате сега?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Рестарт сега!"
  !define MUI_TEXT_FINISH_REBOOTLATER "Желая да рестартирам ръчно по-късно."
  !define MUI_TEXT_FINISH_RUN "Стартирай $(^NameDA)."
  !define MUI_TEXT_FINISH_SHOWREADME "Покажи Readme файла."
  
  !define MUI_TEXT_STARTMENU_TITLE "Изберете папка в Start менюто"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Изберете папка в Start менюто за препратките към програмата."
  !define MUI_INNERTEXT_STARTMENU_TOP "Изберете папка в Start менюто, в която желаете да създадете препратки към файловете на програмата. Можете също така да въведете име, за да създадете нова папка."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не създавай препратки"
  
  !define MUI_TEXT_ABORTWARNING "Сигурни ли сте, че желаете да се прекрати инсталацията на $(^Name)?"  
  
 
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Добре дошли в деинстала-\r\nционния магьосник на $(^NameDA)!"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Този магьосник ще ви упъти при деинсталацията на $(^NameDA).\r\n\r\nПреди да стартирате деинсталацията, уверете се че $(^NameDA) не е стартиран в момента.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Деинсталация на $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Премахване на $(^NameDA) от вашата система."

  !define MUI_UNTEXT_LICENSE_TITLE "Лицензионно споразумение"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Моля прегледайте условията от лицензионното споразумение преди да деинсталирате $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ако приемате условията на споразуменито, натиснете 'Съгласен' за да продължите. Трябва да приемете споразумението, за да деинсталирате $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако приемате условията на споразуменито, click the check box below. Трябва да приемете споразумението, за да деинсталирате $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако приемате условията на споразуменито, отметнете първат опция по-долу. Трябва да приемете споразумението, за да деинсталирате $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Изберете компонентите"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Изберете кои части от $(^NameDA) желаете да бъдат деинсталирани."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Изберете от къде да се деинсталира"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Изберете папка от която искате да се деинсталира $(^NameDA)."
       
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталиране"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Моля изчакайте, докато $(^NameDA) се деинсталира."
    
  !define MUI_UNTEXT_FINISH_TITLE "Приключи"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Деинсталацията приключи успешно."
  
  !define MUI_UNTEXT_ABORT_TITLE "Деинсталирането прекъсна."
  !define MUI_UNTEXT_ABORT_SUBTITLE "Деинсталирането не завърши успешно."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Приключване на деинсталационния магьосник на $(^NameDA)."
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) беше деинсталиран от Вашия компютър.\r\n\r\nНатиснете 'Приключи' за да затворите този магьосник."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Компютърът трябва да се рестартира, за да приключи успешно деинсталацията на $(^NameDA). Искате ли да рестартирате сега?"
  
  !define MUI_UNTEXT_ABORTWARNING "Наистина ли искате да прекъснете деинсталирането на $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END