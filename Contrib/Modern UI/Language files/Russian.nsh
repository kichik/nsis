;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Russian (1049)
;Translated by Nik "brainsucker" Medved - dev by Timon [ timon@front.ru ] + 20030919.
;Updated to v1.68 by THRaSH [ ts2001@hotbox.ru ] & Dmitry Yerokhin [erodim@mail.ru] (040107)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "RUSSIAN"

  !define MUI_LANGNAME "Russian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Вас приветствует мастер установки $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Эта программа установит $(^NameDA) на ваш компьютер.\r\n\r\nПеред началом установки рекомендуется закрыть все запущенные приложения. Это позволит программе установки обновить системные файлы без перезагрузки.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Лицензионное соглашение"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Прочтите условия лицензионного соглашения перед установкой $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Для просмотра остального текста нажмите PageDown."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Если вы принимаете условия соглашения, нажмите кнопку 'Согласен'. Чтобы установить $(^NameDA), вы должны принять условия соглашения."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если вы принимаете условия соглашения, установите флажок ниже. Чтобы установить $(^NameDA), вы должны принять условия соглашения. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если вы принимаете условия соглашения, выберите первый вариант из имеющихся ниже. Чтобы установить $(^NameDA), вы должны принять условия соглашения. $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "Компоненты устанавливаемой программы"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Выберите компоненты $(^NameDA), которые вы хотите установить."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Наведите курсор на компонент, чтобы увидеть его описание."

  !define MUI_TEXT_DIRECTORY_TITLE "Выбор папки установки"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Выберите папку для установки $(^NameDA)."
 
  !define MUI_TEXT_INSTALLING_TITLE "Копирование файлов"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Подождите, пока программа скопирует все необходимые файлы $(^NameDA)."
   
  !define MUI_TEXT_FINISH_TITLE "Установка завершена"
  !define MUI_TEXT_FINISH_SUBTITLE "Установка успешно завершена."
  
  !define MUI_TEXT_ABORT_TITLE "Установка прервана"
  !define MUI_TEXT_ABORT_SUBTITLE "Установка не завершена."
  
  !define MUI_BUTTONTEXT_FINISH "&Готово"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завершение мастера установки $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Установка $(^NameDA) выполнена.\r\n\r\nНажмите 'Готово' для выхода из программы установки."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Чтобы завершить установку $(^NameDA), необходимо перезагрузить компьютер. Сделать это сейчас?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Перезагрузить сейчас"
  !define MUI_TEXT_FINISH_REBOOTLATER "Я выполню перезагрузку позже"
  !define MUI_TEXT_FINISH_RUN "&Запустить $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Показать информацию"
  
  !define MUI_TEXT_STARTMENU_TITLE "Папка в меню 'Пуск'"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Выберите папку в меню 'Пуск' для ярлыков программы."
  !define MUI_INNERTEXT_STARTMENU_TOP "Выберите папку в меню 'Пуск' в которую будут помещены ярлыки устанавливаемой программы. Вы также можете ввести другое имя папки."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не создавать ярлыки"

  !define MUI_TEXT_ABORTWARNING "Вы действительно хотите отменить установку $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Вас приветствует мастер удаления $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Эта программа удалит $(^NameDA) из вашего компьютера.\r\n\r\nПеред началом удаления убедитесь, что программа $(^NameDA) не запущена.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Удаление $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Удаление $(^NameDA) из вашего компьютера."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Лицензионное соглашение"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Прочтите условия лицензионного соглашения перед удалением $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Если вы принимаете условия соглашения, нажмите кнопку 'Согласен'. Чтобы удалить $(^NameDA), нужно принять условия соглашения. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если вы принимаете условия соглашения, установите флажок ниже. Чтобы удалить $(^NameDA), нужно принять условия соглашения. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если вы принимаете условия соглашения, выберите первый вариант из имеющихся ниже. Чтобы удалить $(^NameDA), нужно принять условия соглашения. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Компоненты программы"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Выберите компоненты $(^NameDA), которые вы хотите удалить."
 
  !define MUI_UNTEXT_DIRECTORY_TITLE "Выбор папки для удаления"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Укажите папку, из которой нужно удалить $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Удаление"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Подождите, идёт удаление $(^NameDA)."

  !define MUI_UNTEXT_FINISH_TITLE "Удаление завершено"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Удаление программы успешно завершено."

  !define MUI_UNTEXT_ABORT_TITLE "Удаление прервано"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Удаление произведено не полностью."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Завершение работы мастера удаления $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Программа $(^NameDA) удалена из вашего ПК.\r\n\r\nНажмите 'Готово' для выхода из программы удаления."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Чтобы завершить удаление $(^NameDA), нужно перезагрузить компьютер. Сделать это сейчас?"

  !define MUI_UNTEXT_ABORTWARNING "Вы действительно хотите отказаться от удаления $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END