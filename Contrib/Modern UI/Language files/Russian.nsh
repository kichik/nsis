;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Russian (1049)
;By Nik "brainsucker" Medved ; dev by Timon [ timon@front.ru ] + 20030720

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "RUSSIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Russian"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Нажмите Далее для продолжения установки."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Нажмите Установить для начала процесса установки."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Добро пожаловать в мастер установки ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Эта программа установит ${MUI_PRODUCT} на Ваш компьютер.\r\n\r\nПеред продолжением установки, рекомендуется закрыть все запущенные приложения. Это позволит программе установки обновить системные файлы без перезагрузки.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Лицензионное соглашение"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Прочтите условия лицензионного соглашения перед установкой ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Нажмите PageDown, чтобы перемещать текст вниз."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Если Вы принимаете условия лицензионного соглашения, нажмите на кнопку Согласен. Вы должны принять условия соглашения для установки ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если Вы принимаете условия соглашения, установите отметку в квадратике ниже. Вы должны принять условия соглашения для установки ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если Вы принимаете условия соглашения, выберите первый вариант из имеющихся ниже. Вы должны принять условия соглашения для установки ${MUI_PRODUCT}."  

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Компоненты устанавливаемой программы"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Выберите какие возможности ${MUI_PRODUCT} Вам нужны."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Отметьте те компоненты, которые Вы желаете установить, и снимите отметку для тех, которые Вы устанавливать не желаете."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Подведите курсор к компоненту, чтобы увидеть его описание."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Выбор директории установки"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Выберите директорию для установки ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Программа установки установит ${MUI_PRODUCT} в указанную директорию.$\r$\n$\r$\nДля того, чтобы установить программу в иную директорию, нажмите Обзор и укажите требуемую директорию."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Директория назначения"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Копирование файлов"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Пожалуйста подождите, пока программа установки скопирует все необходимые файлы ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Установка завершена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Установка была успешно завершена."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Установка прервана"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Установка не была успешно завершена."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Завершить"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Завершение мастера установки ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} был установлен на ваш компьютер.\r\n\r\nНажмите Завершить для выхода из программы установки."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Для того, чтобы завешить установку ${MUI_PRODUCT} ваш компьютер должен быть перезагружен. Вы желаете сделать это сейчас?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Перезагрузиться сейчас"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Я хочу перезагрузиться вручную позже"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Запустить ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "&Показать информацию о программе"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Директория в меню Пуск"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Выберите директорию в меню Пуск для ярлыков программы."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Выберите директорию в меню Пуск в которую будут помещены ярлыки для устанавливаемой программы. Вы также можете ввести другое имя для создания новой директории."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Не создавать ярлыки"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Вы уверены, в том, что желаете отменить установку ${MUI_PRODUCT}?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Нажмите Uninstall для начала удаления программы."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Удаление ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Удаление ${MUI_PRODUCT} с вашего компьютера."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Мастер удаления удалит ${MUI_PRODUCT} с вашего компьютера."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Удаление"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Пожалуйста подождите, идёт удаление файлов ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Удаление завершено"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Удаление программы было успешно завершено."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Удаление прервано"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Удаление небыло произведено полностью."

!insertmacro MUI_LANGUAGEFILE_END