;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4

;Language: Russian (1049)
;By Nik Medved (brainsucker). Fixed by Scam.

;--------------------------------
!verbose 3

!ifndef MUI_RUSSIAN_USED

!define MUI_RUSSIAN_USED

  LoadLanguageFile "${NSISDIR}\Contrib\Language files\Russian.nlf"

  !define MUI_RUSSIAN_LANGNAME "Russian" ;Name of the language in the language itself (English, Deutsch, Franзais etc.)

  ;INSTALLER
  Name /LANG=${LANG_RUSSIAN} "${MUI_NAME}"
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_RUSSIAN} "Нажмите PageDown, чтобы просмотреть лицензионное соглашения."
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_RUSSIAN} "Лицензионное Соглашение"  
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_RUSSIAN} "Просмотрите условия Соглашения перед инсталяцией ${MUI_PRODUCT}."
     LangString MUI_INNERTEXT_LICENSE ${LANG_RUSSIAN} "Если Вы принимаете все условия Соглашения, нажмите на кнопку Согласен. Вы должны принять условия Соглашения для инсталяции ${MUI_PRODUCT}."
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_RUSSIAN} "Отметьте те компоненты, которые Вы хотите установить, и снимите отметку для тех, которые Вы устанавливать не хотите. Нажмите Далее для продолжения."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_RUSSIAN} "Выберите компоненты"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_RUSSIAN} "Выберите какие возможности ${MUI_PRODUCT} Вы хотите установить."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_RUSSIAN} "Описание"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_RUSSIAN} "Подведите курсор мыши к компоненту, чтобы увидеть его описание."
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_RUSSIAN} "Инсталятор установит ${MUI_PRODUCT} в следущий каталог.$\r$\n$\r$\nЧтобы установить в этот каталог, нажмите Установить. Чтобы установить в другой каталог, нажмите Обзор и выберите другой каталог."
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_RUSSIAN} "Выберите место установки"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_RUSSIAN} "Выберите каталог для установки ${MUI_PRODUCT}."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_RUSSIAN} "Каталог Назначения"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_RUSSIAN} "Идет установка"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_RUSSIAN} "Пожалуйста подождите, пока идет установка ${MUI_PRODUCT}."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_RUSSIAN} "Завершена"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_RUSSIAN} "Установка успешно завершена."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_RUSSIAN} "Вы уверены, что хотите отменить установку ${MUI_PRODUCT}?"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_RUSSIAN} "${MUI_PRODUCT} ${MUI_VERSION} Установка"
  !endif


  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_RUSSIAN} "Эта программа удалит ${MUI_PRODUCT} с вашего компьютера."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_RUSSIAN} "Удаление ${MUI_PRODUCT}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_RUSSIAN} "Удаление ${MUI_PRODUCT} с Вашего компьютера."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_RUSSIAN} "Удаление"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_RUSSIAN} "Пожалуйста подождите, пока удаляется ${MUI_PRODUCT}."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_RUSSIAN} "Завершено"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_RUSSIAN} "Удаление успешно завершено."
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_RUSSIAN} "${MUI_PRODUCT} ${MUI_VERSION} Установка"
  !endif
    
!endif

!verbose 4