;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Ukrainian (1058)
;By Yuri Holubow, http://www.Nash-Soft.com
;Correct by Osidach Vitaly (Vit_Os2)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Ukrainian"

  !define MUI_LANGNAME "Ukrainian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Ласкаво просимо до Майстра Встановлення $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Дана програма встановить $(^NameDA) на Ваш комп'ютер.\r\n\r\nРекомендовано закрити всі програми перед початком інсталяції. Це дозволить програмі встановлення оновити системні файли без перезавантаження системи.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Ліцензійна Угода"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Будь-ласка перегляньте ліцензію перед встановленням $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Натиснiть PageDown щоб переміститись вниз угоди."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Якщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для встановлення $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Якщо Ви приймаєте всі умови Угоди, встановіть відмітку у квадратику нижче. Ви повинні прийняти умови Угоди для встановлення $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Якщо Ви приймаєте всі умови Угоди, виберіть перший варіант з тих що нижче. Ви повинні прийняти умови Угоди для встановлення $(^NameDA). $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "Оберіть компоненти"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Оберіть компоненти $(^NameDA) якi Ви бажаєте встановити."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Опис"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Пiдведiть вашу мишку до компонента, щоб побачити його опис."

  !define MUI_TEXT_DIRECTORY_TITLE "Оберiть теку встановлення"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Оберiть теку для встановлення $(^NameDA)."
 
  !define MUI_TEXT_INSTALLING_TITLE "Копіювання файлів"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Будь-ласка зачекайте, доки програма встановлення скопіює всі необхідні файли $(^NameDA)."
   
  !define MUI_TEXT_FINISH_TITLE "Встановлення завершено"
  !define MUI_TEXT_FINISH_SUBTITLE "Встановлення успiшно завершено."
  
  !define MUI_TEXT_ABORT_TITLE "Встановлення перервана"
  !define MUI_TEXT_ABORT_SUBTITLE "Встановлення не було успiшно завершено."
  
  !define MUI_BUTTONTEXT_FINISH "&Кінець"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завершення майстра встановлення $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) встановлено на ваш комп'ютер.\r\n\r\nНатисніть Кінець для виходу."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Для того, щоб завершити встановлення $(^NameDA) Ваш комп'ютер повинен перезавантажитися. Зробити це зараз?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Перезавантажитися зараз"
  !define MUI_TEXT_FINISH_REBOOTLATER "Я хочу перезавантажитися власноруч пізніше"
  !define MUI_TEXT_FINISH_RUN "&Запустити $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Показати інформацію про програму"
  
  !define MUI_TEXT_STARTMENU_TITLE "Тека в меню Пуск"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Оберіть теку в меню Пуск для ярликів програми."
  !define MUI_INNERTEXT_STARTMENU_TOP "Оберіть теку в меню Пуск в яку будуть поміщені ярлики для встановленої програми. Ви також можете ввести інше ім'я для створення нової теки."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не створювати ярлики"

  !define MUI_TEXT_ABORTWARNING "Ви впевненнi, що бажаєте покинути встановлення $(^Name)?"

  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Ласкаво просимо до Майстра Видалення $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Даний майстер доможе видалити $(^NameDA).\r\n\r\nПеред початком видалення, перевірте, чи не запущено $(^NameDA).\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Видалення $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Видалення $(^NameDA) з вашого комп'ютера."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Ліцензійна угода"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Прочитайте умови ліцензійної угоди перед видаленням $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Якщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для видалення $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Якщо Ви приймаєте всі умови Угоди, встановіть відмітку у квадратику нижче. Ви повинні прийняти умови Угоди для видалення $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Якщо Ви приймаєте всі умови Угоди, виберіть перший варіант з тих, що нижче. Ви повинні прийняти умови Угоди для видалення $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Компоненти програми"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Оберіть які компоненти $(^NameDA) Ви бажаєте видалити."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Вибір теки для видалення"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Оберіть теку, з якої Ви бажаєте видалити $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Видалення"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Будь-ласка зачекайте, йде видалення файлів $(^NameDA)."
    
  !define MUI_UNTEXT_FINISH_TITLE "Видалення завершено"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Видалення програми було успішно завершено."
  
  !define MUI_UNTEXT_ABORT_TITLE "Видалення перервано"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Видалення не було виконано повністю."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Завершення Майстра Видалення $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) видалений з вашого комп'ютера.\r\n\r\nНатисніть Вихід, щоб закрити Майстра."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Для того, щоб закінчити видалення $(^NameDA) ваш комп'ютер повинен перезавантажитися. Ви бажаєте зробити це зараз?"
  
  !define MUI_UNTEXT_ABORTWARNING "Ви впевненні що бажаєте покинути Майстр Видалення $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END
