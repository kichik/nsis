;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.61

;Language: Russian (1049)
;By Nik Medved (brainsucker). Fixed by Scam (1.5).

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "RUSSIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Russian" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Нажмите Далее для продолжения."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Нажмите Установить для начала инсталяции."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Добро пожаловать в ${MUI_PRODUCT} Мастер Установки"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Эта программа установит ${MUI_PRODUCT} на Ваш компьютер.\r\n\r\nРекомендовано закрыть все остальные приложения перед началом установки. Это позволит програме Установки обновить системные файлы без перезагрузки.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Лицензионное Соглашение"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Просмотрите условия Соглашения перед инсталяцией ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Нажмите PageDown, чтобы просмотреть лицензионное соглашения."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Если Вы принимаете все условия Соглашения, нажмите на кнопку Согласен. Вы должны принять условия Соглашения для инсталяции ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Выберите компоненты"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Выберите какие возможности ${MUI_PRODUCT} Вы хотите установить."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Отметьте те компоненты, которые Вы хотите установить, и снимите отметку для тех, которые Вы устанавливать не хотите."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Подведите курсор мыши к компоненту, чтобы увидеть его описание."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Выберите место установки"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Выберите каталог для установки ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Инсталятор установит ${MUI_PRODUCT} в следущий каталог.$\r$\n$\r$\nЧтобы установить в другой каталог, нажмите Обзор и выберите другой каталог."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Каталог Назначения"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Идет установка"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Пожалуйста подождите, пока идет установка ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Установка Завершена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Установка успешно завершена."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": Завершена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Закрыть"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} был установлен на вашу систему.\r\nНажмите Закрыть чтобы завершить данный Мастер Установки."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Ваша система должна быть перезапущена с целью завершения инсталяции ${MUI_PRODUCT}. Вы хотите перезагрузиться сейчас?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Перезагрузиться сейчас"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Я хочу перезагрузиться вручную позже"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Запустить ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Показать ReadMe"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": Папка в меню Пуск"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Выберите папку в меню Пуск"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Выберите папку в меню Пуск для ярлыков программы."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Выберите папку в меню Пуск в которой вы хотите создать ярлыки программы. Вы также можете ввести другое для создания новой папки."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Не создавать ярлыки"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Вы уверены, что хотите отменить установку ${MUI_PRODUCT}?"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Удаление ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Удаление ${MUI_PRODUCT} с Вашего компьютера."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Эта программа удалит ${MUI_PRODUCT} с вашего компьютера."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Удаление"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Пожалуйста подождите, пока удаляется ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Завершено"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Удаление успешно завершено."
  
!insertmacro MUI_LANGUAGEFILE_END