;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Latvieðu [Latvian] - (1062)
;By Valdis Griíis
;Corrections by Kristaps Meòìelis / x-f (x-f 'AT' inbox.lv)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Latvian"

  !define MUI_LANGNAME "Latvieðu" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Esiet sveicinâti '$(^NameDA)' uzstâdîðanas vednî"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ðis uzstâdîðanas vednis jums palîdzçs veikt '$(^NameDA)' uzstâdîðanu.\r\n\r\nÏoti ieteicams aizvçrt citas programmas pirms ðîs programmas uzstâdîðanas veikðanas. Tas ïaus atjaunot svarîgus sistçmas failus bez datora pârstartçðanas.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licences lîgums"
  !define MUI_TEXT_LICENSE_SUBTITLE "Lûdzu izlasiet licences lîgumu pirms '$(^NameDA)' uzstâdîðanas."
  !define MUI_INNERTEXT_LICENSE_TOP "Spiediet 'Page Down', lai aplûkotu visu lîgumu."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ja piekrîtat licences lîguma noteikumiem, spiediet 'Piekrîtu', lai turpinâtu uzstâdîðanu. Jums ir jâpiekrît licences noteikumiem, lai uzstâdîtu '$(^NameDA)'."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ja piekrîtat licences lîguma noteikumiem, tad atzîmçjiet izvçles rûtiòu. Jums ir jâpiekrît licences noteikumiem, lai uzstâdîtu '$(^NameDA)'. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ja piekrîtat licences lîguma noteikumiem, tad izvçlieties pirmo zemâkesoðo opciju. Jums ir jâpiekrît licences noteikumiem, lai uzstâdîtu '$(^NameDA)'. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Izvçlieties komponentus"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Izvçlieties nepiecieðamâs '$(^NameDA)' sastâvdaïas, kuras uzstâdît."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Apraksts"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Novietojiet peles kursoru uz komponenta, lai tiktu parâdîts tâ apraksts."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Izvçlieties uzstâdîðanas mapi"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Izvçlieties mapi, kurâ uzstâdît '$(^NameDA)'."
  
  !define MUI_TEXT_INSTALLING_TITLE "Notiek uzstâdîðana"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Lûdzu uzgaidiet, kamçr notiek '$(^NameDA)' uzstâdîðana."
  
  !define MUI_TEXT_FINISH_TITLE "Uzstâdîðana pabeigta"
  !define MUI_TEXT_FINISH_SUBTITLE "Uzstâdîðana noritçja veiksmîgi."
  
  !define MUI_TEXT_ABORT_TITLE "Uzstâdîðana atcelta"
  !define MUI_TEXT_ABORT_SUBTITLE "Uzstâdîðana nenoritçja veiksmîgi."
  
  !define MUI_BUTTONTEXT_FINISH "&Pabeigt"
  !define MUI_TEXT_FINISH_INFO_TITLE "Tiek pabeigta '$(^NameDA)' uzstâdîðana"
  !define MUI_TEXT_FINISH_INFO_TEXT "'$(^NameDA)' tika veiksmîgi uzstâdîta jûsu datorâ.\r\n\r\nNospiediet 'Pabeigt', lai aizvçrtu vedni."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Jûsu datoru ir nepiecieðams pârstartçt, lai pabeigtu '$(^NameDA)' uzstâdîðanu. Vai vçlaties pârstartçt datoru tûlît?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Pârstartçt tûlît"
  !define MUI_TEXT_FINISH_REBOOTLATER "Es vçlos pârstartçt pats vçlâk"
  !define MUI_TEXT_FINISH_RUN "P&alaist '$(^NameDA)'"
  !define MUI_TEXT_FINISH_SHOWREADME "Pa&râdît LasiMani failu"
  
  !define MUI_TEXT_STARTMENU_TITLE "Izvçlieties 'Start Menu' folderi"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Izvçlieties 'Start Menu' mapi '$(^NameDA)' saîsnçm."
  !define MUI_INNERTEXT_STARTMENU_TOP "Izvçlieties 'Start Menu' mapi, kurâ tiks izveidotas programmas saîsnes. Varat arî pats izveidot jaunu mapi."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Neveidot saîsnes"
  
  !define MUI_TEXT_ABORTWARNING "Vai tieðâm vçlaties pârtraukt '$(^Name)' uzstâdîðanu?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Esiet sveicinâti '$(^NameDA)' atinstalçðanas vednî"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ðis vednis jums palîdzçs veikt '$(^NameDA)' atinstalçðanu.\r\n\r\nPirms sâkt atinstalçðanas procesu, pârliecinieties, vai '$(^NameDA)' paðlaik nedarbojas.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "'$(^NameDA)' atinstalçðana"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Dzçst '$(^NameDA)' no jûsu datora."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licences lîgums"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Lûdzu izlasiet licences lîgumu pirms '$(^NameDA)' atinstalçðanas."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ja piekrîtat licences noteikumiem, spiediet 'Piekrîtu', lai turpinâtu. Jums ir jâpiekrît licences noteikumiem, lai atinstalçtu '$(^NameDA)'."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ja piekrîtat licences lîguma noteikumiem, tad iezîmçjiet izvçles rûtiòu. Jums ir jâpiekrît licences noteikumiem, lai atinstalçtu '$(^NameDA)'. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ja piekrîtat licences lîguma noteikumiem, tad izvçlieties pirmo zemâkesoðo opciju. Jums ir jâpiekrît licences noteikumiem, lai atinstalçtu '$(^NameDA)'. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Izvçlieties komponentus"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Izvçlieties nepiecieðamâs '$(^NameDA)' sastâvdaïas, kuras atinstalçt."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Izvçlieties atinstalçðanas mapi"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Izvçlieties mapi, no kuras notiks '$(^NameDA)' atinstalçðana."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Notiek atinstalçðana"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Lûdzu uzgaidiet, kamçr '$(^NameDA)' tiek atinstalçta."
    
  !define MUI_UNTEXT_FINISH_TITLE "Atinstalçðana pabeigta"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Atinstalçðana noritçja veiksmîgi."
  
  !define MUI_UNTEXT_ABORT_TITLE "Atinstalçðana atcelta"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Atinstalçðana nenoritçja veiksmîgi."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Tiek pabeigta '$(^NameDA)' atinstalâcija"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "'$(^NameDA)' tika veiksmîgi izdzçsta no jûsu datora.\r\n\r\nNospiediet 'Pabeigt', lai aizvçrtu vedni."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Jûsu datoru nepiecieðams pârstartçt, lai pabeigtu '$(^NameDA)' atinstalçðanu. Vai vçlaties pârstartçt datoru tûlît?"
  
  !define MUI_UNTEXT_ABORTWARNING "Vai tieðâm vçlaties pârtraukt '$(^Name)' atinstalçðanu?"
  
!insertmacro MUI_LANGUAGEFILE_END