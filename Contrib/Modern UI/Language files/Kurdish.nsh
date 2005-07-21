;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Kurdish
;By Rêzan Tovjîn
;Updated by Rêzan Tovjîn (retovjin@hotmail.com)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Kurdish"

  !define MUI_LANGNAME "Kurdish" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "$(^NameDA) Tu bi xêr hatî sêrbaziya sazkirinê"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ev sêrbaz dê di dema sazkirina $(^NameDA) de rêberiya te bike.\r\n\r\nBerî tu dest bi sazkirinê bikî, em pêþniyar dikin tu hemû bernameyên vekirî bigirî. Bi vî rengî beyî tu komputera ji nû ve vekî dê hinek dosiyên pergalê bêpirsgirêk werin sazkirin.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Peymana Lîsansê"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Ji kerema xwe re berî tu bernameya $(^NameDA) saz bikî, peymana lîsansê bixwîne."
  !define MUI_INNERTEXT_LICENSE_TOP "Ji bo dûmahîka peymanê biþkojka 'page down' bitikîne."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Heke tu þertên peymanê dipejirînî, 'Ez Dipejirînim'ê bitikîne. Ji bo sazkirina bernameya $(^NameDA) divê tu þertên peymanê bipejirînî."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Heke tu þertên peymanê dipejirînî, zeviya piþtrastkirinê ya jêrîn dagire. Ji bo tu bikarî bernameya $(^NameDA) saz bikî divê tu þertên peymanê bipejirînî. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Heke tu þertên peymanê dipejirînî, biþkojka erêkirinê ya jêrîn bitikîne. Ji bo sazkirina bernameya $(^NameDA) divê tu þertên peymanê bipejirînî. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Hilbijartina pareyan"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Ji bo sazkirina $(^NameDA) pareyên tu dixwazî hilbijêre."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Dazanîn"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Ji bo tu der barê pareyan de agahiyan bistînî nîþanekê bibe ser pareyekê."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Hilbijartina peldanka armanckirî"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Peldanka tu dixwazî bernameya $(^NameDA) tê de were sazkirin hilbijêre."
  
  !define MUI_TEXT_INSTALLING_TITLE "Tê sazkirin"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Ji kerema xwe re heta sazkirina $(^NameDA) biqede raweste."
  
  !define MUI_TEXT_FINISH_TITLE "Sazkirin Qediya"
  !define MUI_TEXT_FINISH_SUBTITLE "Sazkirin bi serkeftinî qediya."
  
  !define MUI_TEXT_ABORT_TITLE "Sazkirin hate betalkirin"
  !define MUI_TEXT_ABORT_SUBTITLE "Sazkirin be tevahî qediya."
  
  !define MUI_BUTTONTEXT_FINISH "&Biqedîne"
  !define MUI_TEXT_FINISH_INFO_TITLE "Sêrbaziya sazkirina $(^NameDA) diqede."
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) li komputera te hate barkirin.\r\n\r\n'Biqedîne'yê bitikîne û sazkirinê bi dawî bîne."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Ji bo bidawîkirina sazkirina $(^NameDA) divê tu komputerê ji nû ve vekî.Tu dixwazî komputerê ji nû ve vekî?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Ji nû ve veke"
  !define MUI_TEXT_FINISH_REBOOTLATER "Ezê paþê ji nû ve vekim."
  !define MUI_TEXT_FINISH_RUN "Bernameya $(^NameDA) bixebitîne"
  !define MUI_TEXT_FINISH_SHOWREADME "Dosiya min bixwîne/readme &nîþan bide"
  
  !define MUI_TEXT_STARTMENU_TITLE "Hilbijartina Peldanka Pêþeka Destpêkê"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Peldanka pêþeka destpêkê ya ku dê kineriya $(^NameDA) tê de were bikaranîn hilbijêre."
  !define MUI_INNERTEXT_STARTMENU_TOP "Peldanka pêþeka destpêkê ya ku dê kineriya bernameyê tê de were bicihkirin hilbijêre. Tu dikarî bi navekî nû peldankeke nû ava bikî."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Bêyî çêkirina kineriyê bidomîne"
  
  !define MUI_TEXT_ABORTWARNING "Tu bawer î ku dixwazî ji sazkirina $(^Name) derkevî?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Tu bi xêr hatî sêrbaziya rakirina bernameya $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ev sêrbaz ji bo rakirina bernameya $(^NameDA) dê alîkariya te bike.\r\n\r\nBerî tu dest bi rakirina bernameyê bikî, bernameyên vekirî hemûyan bigire. Bi vî rengî dû re tu mecbûr namînî ku komputera xwe bigirî û ji nû ve veki.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Bernameya $(^NameDA) Rake"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Rakirina bernameya $(^NameDA) ji pergala te."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Peymana Lîsansê"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Berî tu bernameya $(^NameDA) ji pergala xwe rakî peymanê bixwîne."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Heke tu þertên peymanê dipejirînî, 'Dipejirînim'ê bitikîne. Ji bo rakirina bernameya  $(^NameDA) divê tu þertên peymanê bipejirînî."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Heke tu þertên peymanê dipejirînî, zeviya erêkirinê ya jêrîn dagire. Ji bo tu bernameya $(^NameDA) ji pergala xwe rakî divê tu peymanê bipejirînî. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Heke tu þertên peymanê dipejirînî, biþkojka erêkirinê ya jêrîn hilbijêre. Ji bo tu bernameya  $(^NameDA) ji pergala xwe rakî divê tu þertên peymanê bipejirînî. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Hilbijartina Pareyan"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Pareya bernameya $(^NameDA) ku tu dixwazî rakî hilbijêre."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Hilbijartina Peldanka Dê Were Rakirin"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Peldanka bernameya $(^NameDA) ku tudixwazî rakî hilbijêre."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Tê rakirin"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Heta bernameya $(^NameDA) ji pergala te were rakirin raweste."
    
  !define MUI_UNTEXT_FINISH_TITLE "Rakirina Bernameyê Biqedîne"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Rakirina bernameyê bi serkeftin pêk hat."
  
  !define MUI_UNTEXT_ABORT_TITLE "Rakirina bernameyê hate betalkirin"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Rakirina bernameyê neqediya."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Sêrbaziya Rakirina Bernameya $(^NameDA) Tê Temamkirin"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Bernameya $(^NameDA) ji pergale hate rakirin.\r\n\r\nJi bo girtina sêrbaz 'biqedîne'yê bitikîne."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Ji bo rakirina bernameya $(^NameDA) biqede divê tu komputera xwe ji nû ve vekî. Tu dixwazî niha komputera te were girtin û ji nû ve dest pê bike?"
  
  !define MUI_UNTEXT_ABORTWARNING "Tu bawer î ku dixwazî dest ji rakirina bernameya $(^Name) berdî?"  
  
!insertmacro MUI_LANGUAGEFILE_END