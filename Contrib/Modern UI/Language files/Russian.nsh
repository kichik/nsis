;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: Russian (1049)
;By Nik Medved (brainsucker). Fixed by Scam.

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "RUSSIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Russian" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Лицензионное Соглашение"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Просмотрите условия Соглашения перед инсталяцией ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Нажмите PageDown, чтобы просмотреть лицензионное соглашения."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Если Вы принимаете все условия Соглашения, нажмите на кнопку Согласен. Вы должны принять условия Соглашения для инсталяции ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Выберите компоненты"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Выберите какие возможности ${MUI_PRODUCT} Вы хотите установить."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "Отметьте те компоненты, которые Вы хотите установить, и снимите отметку для тех, которые Вы устанавливать не хотите. Нажмите Далее для продолжения."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Описание"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Подведите курсор мыши к компоненту, чтобы увидеть его описание."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Выберите место установки"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Выберите каталог для установки ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Инсталятор установит ${MUI_PRODUCT} в следущий каталог.$\r$\n$\r$\nЧтобы установить в этот каталог, нажмите Установить. Чтобы установить в другой каталог, нажмите Обзор и выберите другой каталог."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Каталог Назначения"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Идет установка"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Пожалуйста подождите, пока идет установка ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "Завершена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "Установка успешно завершена."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Вы уверены, что хотите отменить установку ${MUI_PRODUCT}?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Удаление ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Удаление ${MUI_PRODUCT} с Вашего компьютера."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Эта программа удалит ${MUI_PRODUCT} с вашего компьютера."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Удаление"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Пожалуйста подождите, пока удаляется ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Завершено"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Удаление успешно завершено."
  
!insertmacro MUI_LANGUAGEFILE_END