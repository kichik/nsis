;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4

;Language: Ukrainian (1058)
;By Yuri Holubow, Our Soft (http://www.ns.lviv.ua)

;--------------------------------
!verbose 3

!ifndef MUI_UKRAINIAN_USED

!define MUI_UKRAINIAN_USED

  LoadLanguageFile "${NSISDIR}\Contrib\Language files\Ukrainian.nlf"

  !define MUI_UKRAINIAN_LANGNAME "Ukrainian" ;Name of the language in the language itself (English, Deutsch, Franзais etc.)

  ;INSTALLER
  Name /LANG=${LANG_UKRAINIAN} "${MUI_NAME}"
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_UKRAINIAN} "Натиснiть PageDown щоб переглянути залишок угоди."
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_UKRAINIAN} "Лiцензiйна Угода"  
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_UKRAINIAN} "Перегляньте умови Угоди перед iнсталяцiєю ${MUI_PRODUCT}."
     LangString MUI_INNERTEXT_LICENSE ${LANG_UKRAINIAN} "Якщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для iнсталяцiї ${MUI_PRODUCT}."
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_UKRAINIAN} "Вiдмiтьте тi компоненти, якi Ви хочете встановити, i знiмiть вiдмiтку для тих, якi Ви встановлювати не бажаєте. Натиснiть Далi для продовження."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_UKRAINIAN} "Виберiть компоненти"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_UKRAINIAN} "Виберiть якi моливостi ${MUI_PRODUCT} Ви хочете встановити."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_UKRAINIAN} "Опис"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_UKRAINIAN} "Пiдведiть вашу мишку до компонента, щоб побачити його опис."
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_UKRAINIAN} "Iнсталятор встановить ${MUI_PRODUCT} у наступний каталог.$\r$\n$\r$\nЩоб встановити в цей каталог, натиснiть Встановити. Щоб встановити в iнший каталог натиснiть Вибрати i Виберiть iнший каталог."
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_UKRAINIAN} "Виберiть мiсце установки"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_UKRAINIAN} "Виберiть каталог для установки ${MUI_PRODUCT}."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_UKRAINIAN} "Каталог Призначення"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_UKRAINIAN} "Йде установка"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_UKRAINIAN} "Будь-ласка зачекайте, доки йде установка ${MUI_PRODUCT}."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_UKRAINIAN} "Завершена"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_UKRAINIAN} "Установка успiшно завершена."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_UKRAINIAN} "Ви впевненнi що хочете покинути установку ${MUI_PRODUCT}?"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_UKRAINIAN} "${MUI_VERSION} Установка"
  !endif
  

  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_UKRAINIAN} "Ця програма видалить ${MUI_PRODUCT} з вашого комп'ютера."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_UKRAINIAN} "Видалення ${MUI_PRODUCT}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_UKRAINIAN} "Видалення ${MUI_PRODUCT} з вашого комп'ютера."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_UKRAINIAN} "Видалення"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_UKRAINIAN} "Будь-ласка зачекайте доки проходить видалення ${MUI_PRODUCT}."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_UKRAINIAN} "Завершено"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_UKRAINIAN} "Видалення успiшно завершено."
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_UKRAINIAN} "${MUI_VERSION} Установка"
  !endif
    
!endif

!verbose 4