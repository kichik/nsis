;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: Russian (1049)
;By Nik "brainsucker" Medved - dev by Timon [ timon@front.ru ] + 20030919. Updated to v1.67 by THRaSH [ ts2001@hotbox.ru ]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "RUSSIAN"

  !define MUI_LANGNAME "Russian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Добро пожаловать в мастер установки $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Эта программа установит $(^Name) на Ваш компьютер.\r\n\r\nПеред началом установки рекомендуется закрыть все запущенные приложения. Это позволит программе установки обновить системные файлы без перезагрузки.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Лицензионное соглашение"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Прочтите условия лицензионного соглашения перед установкой $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Нажмите PageDown, чтобы перемещать текст вниз."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Если Вы принимаете условия лицензионного соглашения, нажмите кнопку Согласен. Вы должны принять условия соглашения для установки $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если Вы принимаете условия соглашения, установите флажок ниже. Вы должны принять условия соглашения для установки $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если Вы принимаете условия соглашения, выберите первый вариант из имеющихся ниже. Вы должны принять условия соглашения для установки $(^Name). $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "Компоненты устанавливаемой программы"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Выберите какие компоненты $(^Name) Вы желаете установить."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Подведите курсор к компоненту, чтобы увидеть его описание."

  !define MUI_TEXT_DIRECTORY_TITLE "Выбор директории установки"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Выберите директорию для установки $(^Name)."
 
  !define MUI_TEXT_INSTALLING_TITLE "Копирование файлов"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Пожалуйста подождите, пока программа установки скопирует все необходимые файлы $(^Name)."
   
  !define MUI_TEXT_FINISH_TITLE "Установка завершена"
  !define MUI_TEXT_FINISH_SUBTITLE "Установка была успешно завершена."
  
  !define MUI_TEXT_ABORT_TITLE "Установка прервана"
  !define MUI_TEXT_ABORT_SUBTITLE "Установка не была успешно завершена."
  
  !define MUI_BUTTONTEXT_FINISH "&Завершить"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завершение мастера установки $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) установлен на ваш компьютер.\r\n\r\nНажмите Завершить для выхода из программы установки."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Для того, чтобы завешить установку $(^Name) ваш компьютер должен быть перезагружен. Вы желаете сделать это сейчас?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Перезагрузиться сейчас"
  !define MUI_TEXT_FINISH_REBOOTLATER "Я хочу перезагрузиться вручную позже"
  !define MUI_TEXT_FINISH_RUN "&Запустить $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Показать информацию о программе"
  
  !define MUI_TEXT_STARTMENU_TITLE "Директория в меню Пуск"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Выберите директорию в меню Пуск для ярлыков программы."
  !define MUI_INNERTEXT_STARTMENU_TOP "Выберите директорию в меню Пуск в которую будут помещены ярлыки для устанавливаемой программы. Вы также можете ввести другое имя для создания новой директории."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не создавать ярлыки"

  !define MUI_TEXT_ABORTWARNING "Вы уверены, в том, что желаете отменить установку $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Добро пожаловать в мастер удаления $(^Name)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Эта прграмма удалит $(^Name) с вашего компьютера.\r\n\r\nПеред началом удаления убедитесь, что $(^Name) не запущен.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Удаление $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Удаление $(^Name) с вашего компьютера."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Лицензионное соглашение"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Прочтите условия лицензионного соглашения перед удалением $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Если Вы принимаете условия лицензионного соглашения, нажмите на кнопку Согласен. Вы должны принять условия соглашения для удалениня $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если Вы принимаете условия соглашения, установите флажок ниже. Вы должны принять условия соглашения для удаления $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если Вы принимаете условия соглашения, выберите первый вариант из имеющихся ниже. Вы должны принять условия соглашения для удаления $(^Name). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Компоненты программы"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Выберите какие компоненты $(^Name) Вы желаете удалить."
 
  !define MUI_UNTEXT_DIRECTORY_TITLE "Выбор директории для удаления"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Выберите директорию, из которой Вы желаете удалить $(^Name)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Удаление"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Пожалуйста подождите, идёт удаление файлов $(^Name)."

  !define MUI_UNTEXT_FINISH_TITLE "Удаление завершено"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Удаление программы было успешно завершено."

  !define MUI_UNTEXT_ABORT_TITLE "Удаление прервано"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Удаление небыло произведено полностью."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Завершение мастера удаления $(^Name)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) удален с Вашего компьютера.\r\n\r\nНажмите Завершить для выхода из программы удаления."

  !define MUI_UNTEXT_ABORTWARNING "Вы уверены, в том, что желаете отменить удаление $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END