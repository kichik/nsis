;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Bulgarian (1026)
;Translated by Asparouh Kalyandjiev [acnapyx@sbline.net]
;Review and update from v1.63 to v1.68 by Plamen Penkov [plamen71@hotmail.com]
;Updated by Кирил Кирилов (DumpeR) [dumper@data.bg]
;

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Bulgarian"

  !define MUI_LANGNAME "Bulgarian";Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Добре дошли в Съветника\r\nза инсталиране на\r\n$(^NameDA)!"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Той ще инсталира $(^NameDA) на вашия компютър.\r\n\r\nПрепоръчва се да затворите всички други приложения, преди да продължите. Това ще позволи на програмата да обнови някои системни файлове, без да рестартира компютъра.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Лицензионно споразумение"
  !define MUI_TEXT_LICENSE_SUBTITLE "Моля запознайте се Лицензионното споразумение преди да продължите."
  !define MUI_INNERTEXT_LICENSE_TOP "Натиснете клавиша 'Page Down', за да видите останалата част от споразумението."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ако приемате условията на споразумението, натиснете $\"Съгласен$\", за да продължите. Трябва да приемете споразумението, за да инсталирате $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако приемате условията на споразумението, сложете отметка в полето по-долу. Трябва да приемете споразумението, за да инсталирате $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако приемате условията на споразумението, изберете първата опция по-долу. Трябва да приемете споразумението, за да инсталирате $(^NameDA) $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Избор на компоненти"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Изберете кои компоненти на $(^NameDA) искате да бъдат инсталирани."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Преминете с мишката над определен компонент, за да видите описанието му."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Изберете компонент, за да видите описанието му."
  !endif

  !define MUI_TEXT_DIRECTORY_TITLE "Избор на папка за инсталиране"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Изберете папката, в която да се инсталира $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Инсталиране"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Моля изчакайте, докато $(^NameDA) се инсталира."
 
  !define MUI_TEXT_FINISH_TITLE "Инсталирането завърши."
  !define MUI_TEXT_FINISH_SUBTITLE "Инсталирането завърши успешно."

  !define MUI_TEXT_ABORT_TITLE "Инсталирането прекъснато."
  !define MUI_TEXT_ABORT_SUBTITLE "Инсталирането не завърши успешно."
    
  !define MUI_BUTTONTEXT_FINISH "&Край"
  !define MUI_TEXT_FINISH_INFO_TITLE "Затваряне на Съветника за инсталиране на $(^NameDA)."
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) е инсталиран на вашия компютър.\r\n\rНатиснете $\"Край$\", за да затворите Съветника."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Компютърът трябва да бъде рестартиран, за да завърши инсталирането на $(^NameDA). Искате ли да рестартирате сега?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Да, рестартирай сега!"
  !define MUI_TEXT_FINISH_REBOOTLATER "Не, ще рестартирам по-късно."
  !define MUI_TEXT_FINISH_RUN "Стартирай $(^NameDA)."
  !define MUI_TEXT_FINISH_SHOWREADME "Покажи файла ReadMe."
  
  !define MUI_TEXT_STARTMENU_TITLE "Избор на папка в менюто $\"Старт$\""
  !define MUI_TEXT_STARTMENU_SUBTITLE "Изберете папка в менюто $\"Старт$\" за преки пътища към програмата."
  !define MUI_INNERTEXT_STARTMENU_TOP "Изберете папка в менюто $\"Старт$\", в която искате да създадете преки пътища към програмата. Можете също така да въведете име, за да създадете нова папка."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не създавай преки пътища"
  
  !define MUI_TEXT_ABORTWARNING "Сигурни ли сте, че искате да прекратите инсталирането на $(^Name)?"  
  
 
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Добре дошли в Съветника\r\nза деинсталиране на\r\n$(^NameDA)!"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Той ще ви помогне да премахнете $(^NameDA) от вашия компютър.\r\n\r\nПреди да продължите, уверете се че $(^NameDA) не е стартирана в момента.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Деинсталиране на $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Премахване на $(^NameDA) от вашият компютър."

  !define MUI_UNTEXT_LICENSE_TITLE "Лицензионно споразумение"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Моля запознайте се лицензионните условия преди да деинсталирате $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ако приемате условията на споразуменито, натиснете $\"Съгласен$\" за да продължите. Трябва да приемете споразумението, за да деинсталирате $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако приемате условията на споразумението, сложете отметка в полето по-долу. Трябва да приемете споразумението, за да деинсталирате $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако приемате условията на споразуменито, изберете първата опция по-долу. Трябва да приемете споразумението, за да деинсталирате $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Избор на компоненти"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Изберете кои компоненти на $(^NameDA) искате да премахнете."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Избор на папка за деинсталиране"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Изберете папката, от която да се деинсталира $(^NameDA)."
       
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталиране"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Моля изчакайте, докато $(^NameDA) се деинсталира."
    
  !define MUI_UNTEXT_FINISH_TITLE "Край"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Деинсталирането приключи успешно."
  
  !define MUI_UNTEXT_ABORT_TITLE "Деинсталирането прекъснато."
  !define MUI_UNTEXT_ABORT_SUBTITLE "Деинсталирането не завърши успешно."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Приключване на Съветника за деинсталиране на $(^NameDA)."
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) беше деинсталирана от вашия компютър.\r\n\r\nНатиснете 'Край' за да затворите този Съветник."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Компютърът ви трябва да се рестартира, за да приключи успешно деинсталирането на $(^NameDA). Искате ли да рестартирате сега?"
  
  !define MUI_UNTEXT_ABORTWARNING "Сигурни ли сте че искате да прекъснете деинсталирането на $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END