;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Ukrainian (1058)
;By Yuri Holubow, http://www.Nash-Soft.com

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Ukrainian"

  !define MUI_LANGNAME "Ukrainian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Ласкаво просимо до Майстру Установки $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Дана установка встановить $(^NameDA) на Ваш комп'ютер.\r\n\r\nРекомендовано закрити всі програми перед початком інсталяції. Це дозволить установці оновити системні файли без перезавантаження системи.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Ліцензійна Угода"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Будь-ласка перегляньте ліцензію перед установкою $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Натиснiть PageDown щоб переміститися вниз угоди."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Якщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для установки $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Якщо Ви приймаєте всі умови Угоди, встановіть відмітку у квадратику нижче. Ви повинні прийняти умови Угоди для установки $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Якщо Ви приймаєте всі умови Угоди, виберіть перший варіант з тих що нижче. Ви повинні прийняти умови Угоди для установки $(^NameDA). $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "Виберiть компоненти"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Виберiть якi компоненти $(^NameDA) Ви бажаєте встановити."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Опис"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Пiдведiть вашу мишку до компонента, щоб побачити його опис."

  !define MUI_TEXT_DIRECTORY_TITLE "Виберiть директорію установки"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Виберiть директорію для установки $(^NameDA)."
 
  !define MUI_TEXT_INSTALLING_TITLE "Копіювання файлів"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Будь-ласка почекайте, доки програма установки скопіює всі необхідні файли $(^NameDA)."
   
  !define MUI_TEXT_FINISH_TITLE "Установка завершена"
  !define MUI_TEXT_FINISH_SUBTITLE "Установка успiшно завершена."
  
  !define MUI_TEXT_ABORT_TITLE "Установка перервана"
  !define MUI_TEXT_ABORT_SUBTITLE "Установка не була успiшно завершена."
  
  !define MUI_BUTTONTEXT_FINISH "&Кінець"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завершення майстра установки $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) встановлено на ваш комп'ютер.\r\n\r\nНатисніть Кінець для виходу."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Для того, щоб завершити установку $(^NameDA) вашом комп'ютер повинен перезавантажитися. Зробити це зараз?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Перезавантажитися зараз"
  !define MUI_TEXT_FINISH_REBOOTLATER "Я хочу перезавантажитися вручну пізніше"
  !define MUI_TEXT_FINISH_RUN "&Запустити $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Показати інформацію про програму"
  
  !define MUI_TEXT_STARTMENU_TITLE "Директорія в меню Пуск"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Виберіть директорію в меню Пуск для ярликів програми."
  !define MUI_INNERTEXT_STARTMENU_TOP "Виберіть директорію в меню Пуск в яку будуть поміщені ярлики для встановленої програми. Ви також можете ввести інше ім'я для створення нової директорії."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не створювати ярлики"

  !define MUI_TEXT_ABORTWARNING "Ви впевненнi, що бажаєте покинути установку $(^Name)?"

  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Ласкаво просимо до Майстра Видалення $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Даний майстер доможе видалити $(^NameDA).\r\n\r\nПеред початком видалення, перевірте, чи не запусщено $(^NameDA).\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Видалення $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Видалення $(^NameDA) з вашого комп'ютера."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Ліцензійна угода"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Прочитайте умови ліцензійної угоди перед видаленням $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Якщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для видалення $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Якщо Ви приймаєте всі умови Угоди, встановіть відмітку у квадратику нижче. Ви повинні прийняти умови Угоди для видалення $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Якщо Ви приймаєте всі умови Угоди, виберіть перший варіант з тих що нижче. Ви повинні прийняти умови Угоди для видалення $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Компоненти програми"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Виберіть які компоненти $(^NameDA) Ви бажаєте видалити."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Вибір директорії для видалення"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Виберіть директорію, з якої Ви бажаєте видалити $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Видалення"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Будь-ласка зачекайте, йде видалення файлів $(^NameDA)."
    
  !define MUI_UNTEXT_FINISH_TITLE "Видалення завершено"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Видалення програми було успішно завершено."
  
  !define MUI_UNTEXT_ABORT_TITLE "Видалення перервано"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Видалення не було виконано повністю."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Завершення Майстра Видалення $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) видалений з вашого комп'ютера.\r\n\r\nНатисніть Фініш, щоб закрити Майстра."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Для того, щоб закінчити видалення $(^NameDA) ваш комп'ютер повинен перезавантажитися. Ви бажаєте зробити це зараз?"
  
  !define MUI_UNTEXT_ABORTWARNING "Ви впевненні що бажаєте покинути Майстр Видалення $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END