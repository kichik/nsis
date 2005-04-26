;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Russian (1049)
;Translation updated by Dmitry Yerokhin [erodim@mail.ru] (050424)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Russian"

  !define MUI_LANGNAME "Russian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Вас приветствует мастер установки $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Эта программа установит $(^NameDA) на ваш компьютер.\r\n\r\nПеред началом установки рекомендуется закрыть все работающие приложения. Это позволит программе установки обновить системные файлы без перезагрузки компьютера.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Лицензионное соглашение"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Перед установкой $(^NameDA) ознакомьтесь с лицензионным соглашением."
  !define MUI_INNERTEXT_LICENSE_TOP "Для перемещения по тексту используйте клавиши $\"PageUp$\" и $\"PageDown$\"."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Если вы принимаете условия соглашения, нажмите кнопку $\"Принимаю$\". Чтобы установить программу, необходимо принять соглашение."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если вы принимаете условия соглашения, установите флажок ниже. Чтобы установить программу, необходимо принять соглашение. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если вы принимаете условия соглашения, выберите первый вариант из предложенных ниже. Чтобы установить программу, необходимо принять соглашение. $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "Компоненты устанавливаемой программы"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Выберите компоненты $(^NameDA), которые вы хотите установить."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Наведите курсор мыши на название компонента, чтобы прочесть его описание."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Выберите компонент, чтобы увидеть его описание."
  !endif

  !define MUI_TEXT_DIRECTORY_TITLE "Выбор папки установки"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Выберите папку для установки $(^NameDA)."
 
  !define MUI_TEXT_INSTALLING_TITLE "Копирование файлов"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Подождите, идет копирование файлов $(^NameDA)..."
   
  !define MUI_TEXT_FINISH_TITLE "Установка завершена"
  !define MUI_TEXT_FINISH_SUBTITLE "Установка успешно завершена."
  
  !define MUI_TEXT_ABORT_TITLE "Установка прервана"
  !define MUI_TEXT_ABORT_SUBTITLE "Установка не завершена."
  
  !define MUI_BUTTONTEXT_FINISH "&Готово"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завершение работы мастера установки $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Установка $(^NameDA) выполнена.\r\n\r\nНажмите кнопку $\"Готово$\" для выхода из программы установки."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Для завершения установки $(^NameDA) необходимо перезагрузить компьютер. Хотите сделать это сейчас?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Да, перезагрузить ПК сейчас"
  !define MUI_TEXT_FINISH_REBOOTLATER "Нет, я перезагружу ПК позже"
  !define MUI_TEXT_FINISH_RUN "&Запустить $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Показать файл ReadMe"
  
  !define MUI_TEXT_STARTMENU_TITLE "Папка в меню $\"Пуск$\""
  !define MUI_TEXT_STARTMENU_SUBTITLE "Выберите папку в меню $\"Пуск$\" для размещения ярлыков программы."
  !define MUI_INNERTEXT_STARTMENU_TOP "Выберите папку в меню $\"Пуск$\", куда будут помещены ярлыки программы. Вы также можете ввести другое имя папки."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не создавать ярлыки"

  !define MUI_TEXT_ABORTWARNING "Вы действительно хотите отменить установку $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Вас приветствует мастер удаления $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Эта программа удалит $(^NameDA) из вашего компьютера.\r\n\r\nПеред началом удаления убедитесь, что программа $(^NameDA) не запущена.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Удаление $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Удаление $(^NameDA) из компьютера."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Лицензионное соглашение"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Перед удалением $(^NameDA) ознакомьтесь с лицензионным соглашением."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Если вы принимаете условия соглашения, нажмите кнопку $\"Принимаю$\". Для удаления необходимо принять соглашение. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если вы принимаете условия соглашения, установите флажок ниже. Для удаления необходимо принять соглашение. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если вы принимаете условия соглашения, выберите первый вариант из предложенных ниже. Для удаления необходимо принять соглашение. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Компоненты программы"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Выберите компоненты $(^NameDA), которые вы хотите удалить."
 
  !define MUI_UNTEXT_DIRECTORY_TITLE "Выбор папки для удаления"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Укажите папку, из которой нужно удалить $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Удаление"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Подождите, идет удаление файлов $(^NameDA)..."

  !define MUI_UNTEXT_FINISH_TITLE "Удаление завершено"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Удаление программы успешно завершено."

  !define MUI_UNTEXT_ABORT_TITLE "Удаление прервано"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Удаление произведено не полностью."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Завершение работы мастера удаления $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Программа $(^NameDA) удалена из вашего компьютера.\r\n\r\nНажмите кнопку $\"Готово$\"для выхода из программы удаления."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Для завершения удаления $(^NameDA) нужно перезагрузить компьютер. Хотите сделать это сейчас?"

  !define MUI_UNTEXT_ABORTWARNING "Вы действительно хотите отменить удаление $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END