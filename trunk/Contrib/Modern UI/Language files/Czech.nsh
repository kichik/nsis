;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.61

;Language: Czech (1029)
;By T.V. Zuggy (http://zuggy.wz.cz/)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "CZECH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Cesky" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Kliknìte na Další pro pokraèování."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Kliknìte na Instalovat pro spuštìní instalace."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Vítejte v prùvodci instalace programu ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Chystáte se nainstalovat ${MUI_PRODUCT} na svùj poèítaè.\r\n\r\nPøed zaèátkem instalace je doporuèeno zavøít všechny ostatní aplikace. Tímto umožníte instalátoru aktualizovat pøípadné systémové soubory bez nutnosti restartovat systém.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenèní ujednání"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Pøed instalací programu ${MUI_PRODUCT}, prosím, prostudujte licenèní podmínky."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Stisknutím klávesy Page Down posunete text licenèního ujednání."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Jestliže souhlasíte se všemi podmínkami ujednání, zvolte Souhlasím pro pokraèování. Je nutné souhlasit s licenèním ujednáním pro instalaci programu ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Volba souèástí"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Zvolte souèásti programu ${MUI_PRODUCT}, které chcete nainstalovat."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Zaškrtnìte ty souèásti, které chcete nainstalovat a odškrtnìte ty, které nechcete."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Popisek"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pøi pohybu myší nad souèástí programu se zde zobrazí její popisek."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Volba umístìní instalace"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Zvolte adresáø, do kterého chcete nainstalovat program ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} bude nainstalován do následujícího adresáøe.$\r$\n$\r$\nKliknìte na Procházet, pokud chcete tento adresáø zmìnit."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Cílový adresáø"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Instalace"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Vyèkejte, prosím, na dokonèení instalace programu ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Dokonèení instalace"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Instalace probìhla v poøádku."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": dokonèeno"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Dokonèit"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "Právì jste nainstalovali program ${MUI_PRODUCT} do svého systému.\r\nKliknìte na Dokonèit pro uzavøení tohoto prùvodce."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Pro úplné dokonèení instalace programu ${MUI_PRODUCT} je nutné provést restart Vašeho systému. Chcete restartovat ihned?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Restartovat ihned"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Restartovat ruènì pozdìji"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Spustit ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Ukázat Èti-mì"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": Nabídka Start"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Volba umístìní v nabídce Start"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Zvolte položku v nabídce Start pro umístìní zástupcù programu."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Vyberte položku v nabídce Start, ve které chcete vytvoøit zástupce programu. Pokud zadáte neexistující položku, bude vytvoøena nová s Vámi zadaným jménem."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nevytváøet zástupce"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Opravdu chcete ukonèit instalaci programu ${MUI_PRODUCT}?"  


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Kliknìte na Odinstalovat pro spuštìní odinstalace."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Odinstalování programu ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Odstranìní programu ${MUI_PRODUCT} z Vašeho systému."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Tímto odinstalujete program ${MUI_PRODUCT} z Vašeho systému."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Odinstalování"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vyèkejte, prosím, na dokonèení odinstalování programu ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Dokonèeno"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Odinstalování probìhlo v poøádku."

!insertmacro MUI_LANGUAGEFILE_END