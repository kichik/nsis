;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Russian (1049)
;Translated by Nik "brainsucker" Medved - dev by Timon [ timon@front.ru ] + 20030919.
;Updated to v1.68 by THRaSH [ ts2001@hotbox.ru ] & Dmitry Yerokhin [erodim@mail.ru] (040107)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Russian"

  !define MUI_LANGNAME "Russian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Добро пожаловать в мастер установки $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Эта программа установит $(^NameDA) на ваш компьютер.\r\n\r\nПеред началом установки рекомендуется закрыть все работающие приложения. Это позволит программе установки обновить системные файлы без перезагрузки компьютера.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Лицензионное соглашение"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Прочтите условия лицензионного соглашения перед установкой $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Используйте клавиши $\"PageUp$\" и $\"PageDown$\" для перемещения по тексту."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Если вы принимаете условия соглашения, нажмите на кнопку $\"Согласен$\". Это необходимо для установки программы."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если вы принимаете условия соглашения, установите флажок ниже. Это необходимо для установки программы. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если вы принимаете условия соглашения, выберите первый вариант из предложенных ниже. Это необходимо для установки программы. $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "Компоненты устанавливаемой программы"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Выберите компоненты $(^NameDA), которые вы хотите установить."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Установите курсор мыши на название компонента, чтобы прочесть его описание."

  !define MUI_TEXT_DIRECTORY_TITLE "Выбор каталога установки"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Выберите каталог для установки $(^NameDA)."
 
  !define MUI_TEXT_INSTALLING_TITLE "Копирование файлов"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Пожалуйста, подождите, выполняется копирование файлов $(^NameDA) на ваш компьютер..."
   
  !define MUI_TEXT_FINISH_TITLE "Установка завершена"
  !define MUI_TEXT_FINISH_SUBTITLE "Установка успешно завершена."
  
  !define MUI_TEXT_ABORT_TITLE "Установка прервана"
  !define MUI_TEXT_ABORT_SUBTITLE "Установка не завершена."
  
  !define MUI_BUTTONTEXT_FINISH "&Готово"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завершение мастера установки $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Установка $(^NameDA) выполнена.\r\n\r\nНажмите на кнопку $\"Готово$\" для выхода из программы установки."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Чтобы завершить установку $(^NameDA), необходимо перезагрузить компьютер. Вы хотите сделать это сейчас?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Да, перезагрузить компьютер сейчас"
  !define MUI_TEXT_FINISH_REBOOTLATER "Нет, перезагрузить компьютер позже"
  !define MUI_TEXT_FINISH_RUN "&Запустить $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Показать информацию о программе"
  
  !define MUI_TEXT_STARTMENU_TITLE "Папка в меню $\"Пуск$\""
  !define MUI_TEXT_STARTMENU_SUBTITLE "Выберите папку в меню $\"Пуск$\" для размещения ярлыков программы."
  !define MUI_INNERTEXT_STARTMENU_TOP "Выберите папку в меню $\"Пуск$\", куда будут помещены ярлыки программы. Вы также можете ввести другое имя папки."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не создавать ярлыки"

  !define MUI_TEXT_ABORTWARNING "Вы действительно хотите отменить установку $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Вас приветствует мастер удаления $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Эта программа удалит $(^NameDA) с вашего компьютера.\r\n\r\nПеред началом удаления убедитесь, что программа $(^NameDA) не запущена.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Удаление $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Удаление $(^NameDA) с вашего компьютера."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Лицензионное соглашение"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Прочтите условия лицензионного соглашения перед удалением $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Если вы принимаете условия соглашения, нажмите на кнопку $\"Согласен$\". Это необходимо для удаления программы. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Если вы принимаете условия соглашения, установите флажок ниже. Это необходимо для удаления программы. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Если вы принимаете условия соглашения, выберите первый вариант из предложенных ниже. Это необходимо для установки программы. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Компоненты программы"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Выберите компоненты $(^NameDA), которые вы хотите удалить."
 
  !define MUI_UNTEXT_DIRECTORY_TITLE "Выбор каталога для удаления"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Укажите каталог, из которого нужно удалить $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Удаление"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Пожалуйста, подождите, выполняется удаление файлов $(^NameDA) с вашего компьютера..."

  !define MUI_UNTEXT_FINISH_TITLE "Удаление завершено"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Удаление программы успешно завершено."

  !define MUI_UNTEXT_ABORT_TITLE "Удаление прервано"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Удаление произведено не полностью."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Завершение работы мастера удаления $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Программа $(^NameDA) удалена с вашего компьютера.\r\n\r\nНажмите на кнопку $\"Готово$\"для выхода из программы удаления."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Чтобы завершить удаление $(^NameDA), нужно перезагрузить компьютер. Вы хотите сделать это сейчас?"

  !define MUI_UNTEXT_ABORTWARNING "Вы действительно хотите отменить удаление $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END