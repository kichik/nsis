;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Ukrainian (1058)
;By Yuri Holubow, http://www.Nash-Soft.com

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "UKRAINIAN"

  !define MUI_LANGNAME "Ukrainian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Ласкаво просимо до Майстру Установки $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Дана установка встановить $(^Name) на Ваш комп'ютер.\r\n\r\nРекомендовано закрити всі програми перед початком інсталяції. Це дозволить установці оновити системні файли без перезавантаження системи.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Ліцензійна Угода"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Будь-ласка перегляньте ліцензію перед установкою $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Натиснiть PageDown щоб переміститися вниз угоди."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Якщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для установки $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Якщо Ви приймаєте всі умови Угоди, встановіть відмітку у квадратику нижче. Ви повинні прийняти умови Угоди для установки $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Якщо Ви приймаєте всі умови Угоди, виберіть перший варіант з тих що нижче. Ви повинні прийняти умови Угоди для установки $(^Name). $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "Виберiть компоненти"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Виберiть якi компоненти $(^Name) Ви бажаєте встановити."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Опис"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Пiдведiть вашу мишку до компонента, щоб побачити його опис."

  !define MUI_TEXT_DIRECTORY_TITLE "Виберiть директорію установки"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Виберiть директорію для установки $(^Name)."
 
  !define MUI_TEXT_INSTALLING_TITLE "Копіювання файлів"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Будь-ласка почекайте, доки програма установки скопіює всі необхідні файли $(^Name)."
   
  !define MUI_TEXT_FINISH_TITLE "Установка завершена"
  !define MUI_TEXT_FINISH_SUBTITLE "Установка успiшно завершена."
  
  !define MUI_TEXT_ABORT_TITLE "Установка перервана"
  !define MUI_TEXT_ABORT_SUBTITLE "Установка не була успiшно завершена."
  
  !define MUI_BUTTONTEXT_FINISH "&Кінець"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завершення майстра установки $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) встановлено на ваш комп'ютер.\r\n\r\nНатисніть Кінець для виходу."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Для того, щоб завершити установку $(^Name) вашом комп'ютер повинен перезавантажитися. Зробити це зараз?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Перезавантажитися зараз"
  !define MUI_TEXT_FINISH_REBOOTLATER "Я хочу перезавантажитися вручну пізніше"
  !define MUI_TEXT_FINISH_RUN "&Запустити $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Показати інформацію про програму"
  
  !define MUI_TEXT_STARTMENU_TITLE "Директорія в меню Пуск"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Виберіть директорію в меню Пуск для ярликів програми."
  !define MUI_INNERTEXT_STARTMENU_TOP "Виберіть директорію в меню Пуск в яку будуть поміщені ярлики для встановленої програми. Ви також можете ввести інше ім'я для створення нової директорії."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не створювати ярлики"

  !define MUI_TEXT_ABORTWARNING "Ви впевненнi, що бажаєте покинути установку $(^Name)?"

  
  !define MUI_UNTEXT_CONFIRM_TITLE "Видалення $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Видалення $(^Name) з вашого комп'ютера."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Ліцензійна угода"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Прочитайте умови ліцензійної угоди перед видаленням $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "ЕЯкщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для видалення $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Якщо Ви приймаєте всі умови Угоди, встановіть відмітку у квадратику нижче. Ви повинні прийняти умови Угоди для видалення $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Якщо Ви приймаєте всі умови Угоди, виберіть перший варіант з тих що нижче. Ви повинні прийняти умови Угоди для видалення $(^Name). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Компоненти програми"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Виберіть які компоненти $(^Name) Ви бажаєте видалити."
 
  !define MUI_UNTEXT_DIRECTORY_TITLE "Вибір директорії для видалення"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Виберіть директорію, з якої Ви бажаєте видалити $(^Name)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Видалення"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Будь-ласка зачекайте, йде видалення файлів $(^Name)."

  !define MUI_UNTEXT_FINISH_TITLE "Видалення завершено"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Видалення програми було успішно завершено."

  !define MUI_UNTEXT_ABORT_TITLE "Видалення перервано"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Видалення не було виконано повністю."
  
!insertmacro MUI_LANGUAGEFILE_END