;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Latvieðu [Latvian] - (1062)
;By Valdis Griíis

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "LATVIAN"

  !define MUI_LANGNAME "Latvieðu" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Esiet sveicinâti $(^NameDA) uzstâdîðanas vednî"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ðis instalâcijas vednis jums palîdzçs veikt $(^NameDA) instalâciju.\r\n\r\nÏoti ieteicams aizvçrt citas aplikâcijas pirms instalâcijas veikðanas. Tas ïaus atjaunot svarîgus sistçmas failus bez datora restartçðanas.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licences lîgums"
  !define MUI_TEXT_LICENSE_SUBTITLE "Lûdzu izlasiet licences lîgumu pirms $(^NameDA) instalçðanas."
  !define MUI_INNERTEXT_LICENSE_TOP "Nospiediet 'Page Down', lai aplûkotu visu lîgumu."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ja pieòemat licences lîguma noteikumus, nospiediet 'Piekrîtu', lai turpinâtu. Jums ir jâpieòem licences noteikumi, lai uzinstalçtu $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ja pieòemat licences lîguma noteikumus, tad ieíeksçjiet izvçles rûtiòu. Jums ir jâpieòem licences noteikumi, lai uzinstalçtu $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ja pieòemat licences lîguma noteikumus, tad izvçlieties pirmo opciju apakðâ. Jums ir jâpieòem licences noteikumi, lai uzinstalçtu $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Izvçlieties komponentus"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Izvçlieties vajadzîgâs $(^NameDA) sastâvdaïas, kuras tiks instalçtas"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Apraksts"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Uzbrauciet ar peles kursoru uz komponenta, lai tiktu parâdîts tâ apraksts."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Izvçlieties instalâcijas folderi"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Izvçlieties folderi, kur tiks instalçts $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Notiek instalçðana"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Lûdzu uzgaidiet, kamçr norit $(^NameDA) instalâcija."
  
  !define MUI_TEXT_FINISH_TITLE "Instalâcija pabeigta"
  !define MUI_TEXT_FINISH_SUBTITLE "Uzstâdîðana noritçja veiksmîgi."
  
  !define MUI_TEXT_ABORT_TITLE "Instalâcija atcelta"
  !define MUI_TEXT_ABORT_SUBTITLE "Uzstâdîðana nenoritçja veiksmîgi."
  
  !define MUI_BUTTONTEXT_FINISH "&Pabeigt"
  !define MUI_TEXT_FINISH_INFO_TITLE "Tiek pabeigta $(^NameDA) instalâcija"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) tika veiksmîgi uzinstalçts jûsu datorâ.\r\n\r\nNospiediet 'Pabeigt', lai aizvçrtu vedni."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Jûsu datoram ir nepiecieðams restarts, lai pabeigtu $(^NameDA) instalâciju. Vai vçlaties restartçt datoru tagad?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Restartçt tagad"
  !define MUI_TEXT_FINISH_REBOOTLATER "Vçlos pats restartçt vçlâk"
  !define MUI_TEXT_FINISH_RUN "P&alaist $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Pa&râdît Readme"
  
  !define MUI_TEXT_STARTMENU_TITLE "Izvçlieties 'Start Menu' folderi"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Izvçlieties 'Start Menu' folderi priekð $(^NameDA) saîsnçm."
  !define MUI_INNERTEXT_STARTMENU_TOP "Izvçlieties 'Start Menu' folderi, kurâ tiks izveidotas programmas saîsnes. Varat arî paði izveidot jaunu folderi."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Neveidot saîsnes"
  
  !define MUI_TEXT_ABORTWARNING "Vai tieðâm vçlaties iziet no $(^Name) uzstâdîðanas?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Esiet sveicinâti $(^NameDA) atinstalçðanas vednî"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ðis vednis jums palîdzçs veikt $(^NameDA) atinstalçðanu.\r\n\r\nPirms sâkt atinstalçt, pârliecinieties, vai $(^NameDA) paðlaik nav atvçrta.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Atinstalçt $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Nodzçst $(^NameDA) no jûsu datora."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licences lîgums"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Lûdzu izlasiet licences lîgumu pirms $(^NameDA) atinstalçðanas."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ja pieòemat licences noteikumus, nospiediet 'Pieòemu', lai turpinâtu. Jums ir jâpieòem licences noteikumi, lai atinstalçtu $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ja pieòemat licences lîguma noteikumus, tad ieíeksçjiet izvçles rûtiòu. Jums ir jâpieòem licences noteikumi, lai atinstalçtu $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ja pieòemat licences lîguma noteikumus, tad izvçlieties pirmo opciju apakðâ. Jums ir jâpieòem licences noteikumi, lai atinstalçtu $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Izvçlieties komponentus"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Izvçlieties vajadzîgâs $(^NameDA) sastâvdaïas, kuras tiks atinstalçtas."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Izvçlieties atinstalâcijas folderi"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Izvçlieties folderi, no kura notiks $(^NameDA) atinstalâcija."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Notiek atinstalçðana"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Lûdzu uzgaidiet, kamçr $(^NameDA) tiek atinstalçta."
    
  !define MUI_UNTEXT_FINISH_TITLE "Atinstalâcija pabeigta"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Atinstalâcija noritçja veiksmîgi."
  
  !define MUI_UNTEXT_ABORT_TITLE "Atinstalâcija atcelta"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Atinstalçðana nenoritçja veiksmîgi."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Tiek pabeigta $(^NameDA) atinstalâcija"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) tika veiksmîgi izdzçsta no jûsu datora.\r\n\r\nNospiediet 'Pabeigt', lai aizvçrtu vedni."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Jûsu datoram ir nepiecieðams restarts, lai pabeigtu $(^NameDA) atinstalâciju. Vai vçlaties restartçt datoru tagad?"
  
  !define MUI_UNTEXT_ABORTWARNING "Vai tieðâm vçlaties iziet no $(^Name) atinstalçðanas?"
  
!insertmacro MUI_LANGUAGEFILE_END