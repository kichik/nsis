;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: Ukrainian (1058)
;By Yuri Holubow, Our Soft (http://www.ns.lviv.ua)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "UKRAINIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Ukrainian" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Лiцензiйна Угода"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Перегляньте умови Угоди перед iнсталяцiєю ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Натиснiть PageDown щоб переглянути залишок угоди."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Якщо Ви приймаєте всi умови Угоди, натиснiть на кнопку Згоден. Ви повиннi прийняти умови Угоди для iнсталяцiї ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Виберiть компоненти"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Виберiть якi моливостi ${MUI_PRODUCT} Ви хочете встановити."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "Вiдмiтьте тi компоненти, якi Ви хочете встановити, i знiмiть вiдмiтку для тих, якi Ви встановлювати не бажаєте. Натиснiть Далi для продовження."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Опис"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Пiдведiть вашу мишку до компонента, щоб побачити його опис."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Виберiть мiсце установки"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Виберiть каталог для установки ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Iнсталятор встановить ${MUI_PRODUCT} у наступний каталог.$\r$\n$\r$\nЩоб встановити в цей каталог, натиснiть Встановити. Щоб встановити в iнший каталог натиснiть Вибрати i Виберiть iнший каталог."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Каталог Призначення"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Йде установка"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Будь-ласка зачекайте, доки йде установка ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "Завершена"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "Установка успiшно завершена."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Ви впевненнi що хочете покинути установку ${MUI_PRODUCT}?"
  

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Видалення ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Видалення ${MUI_PRODUCT} з вашого комп'ютера."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Ця програма видалить ${MUI_PRODUCT} з вашого комп'ютера."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Видалення"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Будь-ласка зачекайте доки проходить видалення ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Завершено"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Видалення успiшно завершено."
   
!insertmacro MUI_LANGUAGEFILE_END