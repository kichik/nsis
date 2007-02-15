;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Czech (1029)
;By SELiCE (ls@selice.cz - http://ls.selice.cz)
;Corrected by Ondøej Vaniš - http://www.vanis.cz/ondra

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Czech"

  !define MUI_LANGNAME "Cesky" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Vítejte v prùvodci instalace programu $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Tento prùvodce Vás provede instalací $(^NameDA).\r\n\r\nPøed zaèátkem instalace je doporuèeno zavøít všechny ostatní aplikace. Toto umožní aktualizovat dùležité systémové soubory bez restartování Vašeho poèítaèe.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licenèní ujednání"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Pøed instalací programu $(^NameDA) si prosím prostudujte licenèní podmínky."
  !define MUI_INNERTEXT_LICENSE_TOP "Stisknutím klávesy Page Down posunete text licenèního ujednání."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Jestliže souhlasíte se všemi podmínkami ujednání, zvolte 'Souhlasím' pro pokraèování. Pro instalaci programu $(^NameDA) je nutné souhlasit s licenèním ujednáním."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jestliže souhlasíte se všemi podmínkami ujednání, zaškrtnìte níže uvedenou volbu. Pro instalaci programu $(^NameDA) je nutné souhlasit s licenèním ujednáním. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jestliže souhlasíte se všemi podmínkami ujednání, zvolte první z možností uvedených níže. Pro instalaci programu $(^NameDA) je nutné souhlasit s licenèním ujednáním. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Volba souèástí"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Zvolte souèásti programu $(^NameDA), které chcete nainstalovat."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Popis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pøi pohybu myší nad instalátorem programu se zobrazí její popis."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Zvolte umístìní instalace"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Zvolte složku, do které bude program $(^NameDA) nainstalován."
  
  !define MUI_TEXT_INSTALLING_TITLE "Instalace"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vyèkejte, prosím, na dokonèení instalace programu $(^NameDA)."
  
  !define MUI_TEXT_FINISH_TITLE "Instalace dokonèena"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalace probìhla v poøádku."
  
  !define MUI_TEXT_ABORT_TITLE "Instalace pøerušena"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalace nebyla dokonèena."
  
  !define MUI_BUTTONTEXT_FINISH "&Dokonèit"
  !define MUI_TEXT_FINISH_INFO_TITLE "Dokonèení prùvodce programu $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Program $(^NameDA) byl nainstalován na Váš poèítaè.\r\n\r\nKliknìte 'Dokonèit' pro ukonèení prùvodce."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Pro dokonèení instalace programu $(^NameDA) je nutno restartovat poèítaè. Chcete restatovat nyní?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Restartovat nyní"
  !define MUI_TEXT_FINISH_REBOOTLATER "Restartovat ruènì pozdìji"
  !define MUI_TEXT_FINISH_RUN "&Spustit program $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Zobrazit Èti-mne"
  
  !define MUI_TEXT_STARTMENU_TITLE "Zvolte složku v Nabídce Start"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Zvolte složku v Nabídce Start pro zástupce programu $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Zvolte složku v Nabídce Start, ve které chcete vytvoøit zástupce programu. Mùžete také zadat nové jméno pro vytvoøení nové složky."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nevytváøet zástupce"
  
  !define MUI_TEXT_ABORTWARNING "Opravdu chcete ukonèit instalaci programu $(^Name)?"  
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Vítejte v $(^NameDA) odinstalaèním prùvodci"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Tento prùvodce Vás provede odinstalací $(^NameDA).\r\n\r\nPøed zaèátkem odinstalace, se pøesvìdète, že $(^NameDA) není spuštìn.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Odinstalovat program $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Odebrat program $(^NameDA) z Vašeho poèítaèe."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licenèní ujednání"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Pøed odinstalováním programu $(^NameDA) si prosím prostudujte licenèní podmínky."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Jestliže souhlasíte se všemi podmínkami ujednání, zvolte 'Souhlasím' pro pokraèování. Pro odinstalování programu $(^NameDA) je nutné souhlasit s licenèním ujednáním."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jestliže souhlasíte se všemi podmínkami ujednání, zaškrtnìte níže uvedenou volbu. Pro odinstalování programu $(^NameDA) je nutné souhlasit s licenèním ujednáním. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jestliže souhlasíte se všemi podmínkami ujednání, zvolte první z níže uvedených možností. Pro odinstalování programu $(^NameDA) je nutné souhlasit s licenèním ujednáním. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Volba souèástí"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Zvolte souèásti programu $(^NameDA), které chcete odinstalovat."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Zvolte umístìní odinstalace"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Zvolte složku, ze které bude program $(^NameDA) odinstalován."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Odinstalace"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vyèkejte, prosím, na dokonèení odinstalace programu $(^NameDA)."
    
  !define MUI_UNTEXT_FINISH_TITLE "Odinstalace dokonèena"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Odinstalace probìhla v poøádku."
  
  !define MUI_UNTEXT_ABORT_TITLE "Odinstalace pøerušena"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Odinstalace nebyla dokonèena."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Dokonèuji odinstalaèního prùvodce $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) byl odinstalován z Vašeho poèítaèe.\r\n\r\nKliknìte na 'Dokonèit' pro ukonèení tohoto prùvodce."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Pro dokonèení odinstalace $(^NameDA) musí být Váš poèítaè restartován. Chcete restartovat nyní?"

  !define MUI_UNTEXT_ABORTWARNING "Skuteènì chcete ukonèit odinstalaci $(^Name)?"  
  
!insertmacro MUI_LANGUAGEFILE_END