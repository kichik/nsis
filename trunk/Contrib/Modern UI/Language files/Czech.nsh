;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Czech (1029)
;By T.V. Zuggy (http://zuggy.wz.cz/)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "CZECH"

  !define MUI_LANGNAME "Cesky" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Vítejte v prùvodci instalace programu $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Chystáte se nainstalovat $(^Name) na svùj poèítaè.\r\n\r\nPøed zaèátkem instalace je doporuèeno zavøít všechny ostatní aplikace. Tímto umožníte instalátoru aktualizovat pøípadné systémové soubory bez nutnosti restartovat systém.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licenèní ujednání"
  !define MUI_TEXT_LICENSE_SUBTITLE "Pøed instalací programu $(^Name), prosím, prostudujte licenèní podmínky."
  !define MUI_INNERTEXT_LICENSE_TOP "Stisknutím klávesy Page Down posunete text licenèního ujednání."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Jestliže souhlasíte se všemi podmínkami ujednání, zvolte Souhlasím pro pokraèování. Je nutné souhlasit s licenèním ujednáním pro instalaci programu $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jestliže souhlasíte se všemi podmínkami ujednání, zaškrtnìte níže uvedenou volbu. Je nutné souhlasit s licenèním ujednáním pro instalaci programu $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jestliže souhlasíte se všemi podmínkami ujednání, zvolte první z možností uvedených níže. Je nutné souhlasit s licenèním ujednáním pro instalaci programu $(^Name)."

  !define MUI_TEXT_COMPONENTS_TITLE "Volba souèástí"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Zvolte souèásti programu $(^Name), které chcete nainstalovat."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Popisek"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pøi pohybu myší nad souèástí programu se zde zobrazí její popisek."

  !define MUI_TEXT_DIRECTORY_TITLE "Volba umístìní instalace"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Zvolte adresáø, do kterého chcete nainstalovat program $(^Name)."

  !define MUI_TEXT_INSTALLING_TITLE "Instalace"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vyèkejte, prosím, na dokonèení instalace programu $(^Name)."

  !define MUI_TEXT_FINISH_TITLE "Dokonèení instalace"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalace probìhla v poøádku."

  !define MUI_TEXT_ABORT_TITLE "Instalace pøerušena"
  !define MUI_TEXT_ABORT_SUBTITLE "UPOZORNÌNÍ: Instalace nebyla dokonèena."

  !define MUI_BUTTONTEXT_FINISH "&Dokonèit"
  !define MUI_TEXT_FINISH_INFO_TITLE "Dokonèení prùvodce instalace programu $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Právì jste nainstalovali program $(^Name) do svého systému.\r\nKliknìte na Dokonèit pro uzavøení tohoto prùvodce."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Pro úplné dokonèení instalace programu $(^Name) je nutné provést restart Vašeho systému. Chcete restartovat ihned?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Restartovat ihned"
  !define MUI_TEXT_FINISH_REBOOTLATER "Restartovat ruènì pozdìji"
  !define MUI_TEXT_FINISH_RUN "&Spustit $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Ukázat Èti-mì"

  !define MUI_TEXT_STARTMENU_TITLE "Volba umístìní v nabídce Start"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Zvolte položku v nabídce Start pro umístìní zástupcù programu."
  !define MUI_INNERTEXT_STARTMENU_TOP "Vyberte položku v nabídce Start, ve které chcete vytvoøit zástupce programu. Pokud zadáte neexistující položku, bude vytvoøena nová s Vámi zadaným jménem."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nevytváøet zástupce"

  !define MUI_TEXT_ABORTWARNING "Opravdu chcete ukonèit instalaci programu $(^Name)?"


  !define MUI_UNTEXT_CONFIRM_TITLE "Odinstalování programu $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Odstranìní programu $(^Name) z Vašeho systému."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Odinstalování"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vyèkejte, prosím, na dokonèení odinstalování programu $(^Name)."

  !define MUI_UNTEXT_FINISH_TITLE "Dokonèení odinstalace"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Odinstalování probìhlo v poøádku."

  !define MUI_UNTEXT_ABORT_TITLE "Odinstalování pøerušeno"
  !define MUI_UNTEXT_ABORT_SUBTITLE "UPOZORNÌNÍ: Odinstalování nebylo dokonèeno."

!insertmacro MUI_LANGUAGEFILE_END