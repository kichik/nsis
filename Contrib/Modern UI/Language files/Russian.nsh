;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: Russian (1049)
;By Nik Medved (brainsucker)

;--------------------------------

!ifndef MUI_RUSSIAN_USED

!define MUI_RUSSIAN_USED

  !define MUI_RUSSIAN_LANGNAME "Russian" ;Name of the language in the language itself (English, Deutsch, Franзais etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_RUSSIAN} "Нажимте PageDown чтобы просмотреть остаток соглашения."
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_RUSSIAN} "Лицензионное Соглашение"  
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_RUSSIAN} "Просмотрите условия Соглашения перед инсталяцией ${NAME}."
     LangString MUI_INNERTEXT_LICENSE ${LANG_RUSSIAN} "Если вы принимаете все условия Соглашения, нажмите на кнопку Согласен. Вы должны принять условия Соглашения для инсталяции ${NAME}."
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_RUSSIAN} "Отметьте те компоненты, которые вы хотите установить, и снимите отметку для тех, которые вы устанавливать не хотите. Нажмите Далее для продолжения."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_RUSSIAN} "Выберите компоненты"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_RUSSIAN} "Выберите какие вохможности ${NAME} вы хотите установить."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_RUSSIAN} "Описание"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_RUSSIAN} "Подведите вашу мышку к компоненту, чтобы увидеть его описание."
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_RUSSIAN} "Инсталятор установит ${NAME} в следущий каталог.$\r$\n$\r$\nЧтобы установить в этот каталог, нажмите Установить. Чтобы установить в другой каталог нажмите Выбрать и выберите другой каталог." " "
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_RUSSIAN} "Выберите место установки"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_RUSSIAN} "Выберите каталог для установки ${NAME}."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_RUSSIAN} "Каталог Назначения"
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_RUSSIAN} "&Далее >"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_RUSSIAN} "Идет установка"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_RUSSIAN} "Пожалуйтся подождите, пока идет установка ${NAME}."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_RUSSIAN} "Завершена"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_RUSSIAN} "Установка успешно завершена."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_MSGTEXT_ABORTWARNING ${LANG_RUSSIAN} "Вы уверены что хотите покинуть установку ${NAME}?"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_BUTTONTEXT_BACK ${LANG_RUSSIAN} "< &Назад"
    LangString MUI_BUTTONTEXT_NEXT ${LANG_RUSSIAN} "&Далее >"
    LangString MUI_BUTTONTEXT_CANCEL ${LANG_RUSSIAN} "Отмена"
    LangString MUI_BUTTONTEXT_INSTALL ${LANG_RUSSIAN} "&Установить"
  !endif


  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_RUSSIAN} "Эта программа удалит ${NAME} с вашего компьютера."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_RUSSIAN} "Удаление ${NAME}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_RUSSIAN} "Удаление ${NAME} с вашего компьютера."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_RUSSIAN} "Удаление"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_RUSSIAN} "Пожалуйста подождите пока происходит удаление ${NAME}."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_RUSSIAN} "Завершено"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_RUSSIAN} "Удаление успешно завершено."
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_RUSSIAN} "&Далее >"
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_BUTTONTEXT_BACK ${LANG_RUSSIAN} "< &Назад"
    LangString un.MUI_BUTTONTEXT_NEXT ${LANG_RUSSIAN} "&Далее >"
    LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_RUSSIAN} "Отмена"
    LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_RUSSIAN} "&Удалить"
  !endif  
    
!endif