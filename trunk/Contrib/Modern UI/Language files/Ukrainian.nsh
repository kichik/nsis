;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.6

;Language: Ukrainian (1058)
;By Yuri Holubow http://www.ns.lviv.ua/home

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "UKRAINIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Ukrainian" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Натисніть Далі для продовження."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Натисніть Встановити для посатку установки."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_TITLE "Ласкаво просимо до ${MUI_PRODUCT} Майстру Установки"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO "Дана установка встановить ${MUI_PRODUCT} на ваш комп'ютер.\r\n\r\nРекомендовано закрити всі програми перед початком інсталяції. Це дозволить установці оновити системні файли без перезавантаження системи.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Ліцензійна Угода"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Будь-ласка перегляньте ліцензію перед установкою ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Натиснiть PageDown щоб переглянути залишок угоди."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Якщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для iнсталяцiї ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Виберiть компоненти"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Виберiть якi моливостi ${MUI_PRODUCT} Ви хочете встановити."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Вiдмiтьте тi компоненти, якi Ви хочете встановити, i знiмiть вiдмiтку для тих, якi Ви встановлювати не бажаєте."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Опис"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Пiдведiть вашу мишку до компонента, щоб побачити його опис."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Виберiть мiсце установки"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Виберiть каталог для установки ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Iнсталятор встановить ${MUI_PRODUCT} у наступний каталог.$\r$\n$\r$\nЩоб встановити в iнший каталог натиснiть Вибрати i виберiть iнший каталог."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Каталог Призначення"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Йде установка"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Будь-ласка зачекайте, доки йде установка ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Установка Завершена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Установка успiшно завершена."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": Завершено"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Кінець"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO "${MUI_PRODUCT} встановлено на ваш комп'ютер.\r\nНатисніть Кінець для виходу."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Ваша система мусить перезавантажитися для повної установки ${MUI_PRODUCT}. Do you want to reboot now?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Перезавантажити"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Перезавантажити Пізніше"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Запустити ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Показати Readme"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": Папка Меню Пуск"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Виберіть папку Меню Пуск"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Виберіть папку у Меню Пуск для створення ярликів."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Виберіть папку у меню Пуск для створення ярликів для програми. Для створення нової папки введіть її назву."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Не створювати ярлики"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Ви впевненнi що хочете покинути установку ${MUI_PRODUCT}?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Видалення ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Видалення ${MUI_PRODUCT} з вашої системи."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Дана програма видалить ${MUI_PRODUCT} з вашої системи."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Видалення"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Будь-ласка зачекайте, ${MUI_PRODUCT} видаляється."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Закінчено"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Видалення пройшло успішно."
  
!insertmacro MUI_LANGUAGEFILE_END