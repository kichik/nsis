;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Basque (1069)
;By Iñaki San Vicente

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Basque"

  !define MUI_LANGNAME "Euskera" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Ongi etorri $(^NameDA) -ren instalazio programara"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Programa honek $(^NameDA) zure ordenagailuan instalatuko du.\r\n\r\nAholkatzen da instalazioarekin hasi aurretik beste aplikazio guztiak ixtea. Honek sistemarekin erlazionatuta dauden fitxategien eguneratzea ahalbidetuko du, ordenagailua berrabiarazi beharrik izan gabe.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Lizentzia hitzarmena"
  !define MUI_TEXT_LICENSE_SUBTITLE "Mesedez aztertu lizentziaren baldintzak $(^NameDA) instalatu aurretik."
  !define MUI_INNERTEXT_LICENSE_TOP "Sakatu Av Pág hitzarmenaren gainontzeko atalak ikusteko."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Baldintzak onartzen badituzu, sakatu Onartu aurrera egiteko. Hitzarmena onartzea ezinbestekoa da $(^NameDA) instalatzeko."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Baldintzak onartzen badituzu, nabarmendu azpiko laukitxoa. Hitzarmena onartzea ezinbestekoa da $(^NameDA) instalatzeko. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Baldintzak onartzen badituzu, hautatu azpian lehen aukera. Hitzarmena onartzea ezinbestekoa da $(^NameDA) instalatzeko. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Osagaien hautatzea"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Hautatu $(^NameDA)-ren zein ezaugarri instalatu nahi duzun."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Azalpena"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Jarri sagua osagai baten gainean dagokion azalpena ikusteko."
  !else
   !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Hautatu osagai bat bere azalpena ikusteko."
  !endif
  	   
  !define MUI_TEXT_DIRECTORY_TITLE "Hautatu instalazioaren lekua"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Aukeratu $(^NameDA) instalatzeko karpeta."
  
  !define MUI_TEXT_INSTALLING_TITLE "Instalatzen"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Mesedez itxoin $(^NameDA) instalatzen den bitartean."
  
  !define MUI_TEXT_FINISH_TITLE "Instalazioa burututa"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalazioa zuzen burutu da."

  !define MUI_TEXT_ABORT_TITLE "Instalazioa ezeztatua"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalazioa ez da zuzen burutu."

  !define MUI_BUTTONTEXT_FINISH "&Amaitu"
  !define MUI_TEXT_FINISH_INFO_TITLE "$(^NameDA)-ren instalazio laguntzailea osatzen"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) zure sisteman instalatu da.\r\n\r\nSakatu Amaitu laguntzaile hau ixteko."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Zure sistema berrabiarazi behar duzu $(^NameDA)-ren instalazioa osatzeko. Orain Berrabiarazi nahi duzu?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Berrabiarazi orain"
  !define MUI_TEXT_FINISH_REBOOTLATER "Neuk berrabiarazi geroago"
  !define MUI_TEXT_FINISH_RUN "&Exekutatu $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Ikusi Readme.txt"

  !define MUI_TEXT_STARTMENU_TITLE "Aukeratu Hasiera Menuko karpeta"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Aukeratu Hasiera Menuko karpeta bat $(^NameDA)-ren lasterbideentzako."
  !define MUI_INNERTEXT_STARTMENU_TOP "Aukeratu Hasiera Menuko karpeta bat, non programaren lasterbideak instalatu nahi dituzun. Karpeta berri bat sortzeko izen bat ere adierazi dezakezu."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ez sortu lasterbiderik"
  
  !define MUI_TEXT_ABORTWARNING "Ziur zaude $(^Name)-ren instalaziotik irten nahi duzula?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Ongi etorri $(^NameDA) -ren ezabaketa programara"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Laguntzaile honek $(^NameDA)-ren ezabaketa prozesuan zehar gidatuko zaitu.\r\n\r\nEzabaketa hasi aurretik, ziurtatu $(^NameDA) martxan ez dagoela .\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Ezabatu $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "$(^NameDA) zure sistematik ezabatzen du."

  !define MUI_UNTEXT_LICENSE_TITLE "Lizentzia hitzarmena"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Mesedez aztertu lizentziaren baldintzak $(^NameDA) ezabatu aurretik."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Baldintzak onartzen badituzu, sakatu Onartu aurrera egiteko. Hitzarmena onartzea ezinbestekoa da $(^NameDA) ezabatzeko."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Baldintzak onartzen badituzu, nabarmendu azpiko laukitxoa. Hitzarmena onartzea ezinbestekoa da $(^NameDA) ezabatzeko. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Baldintzak onartzen badituzu, hautatu azpian lehen aukera. Hitzarmena onartzea ezinbestekoa da $(^NameDA) ezabatzeko. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Osagaien hautatzea"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Hautatu $(^NameDA)-ren zein ezaugarri ezabatu nahi duzun."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Aukeratu ezabatuko den karpeta"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Aukeratu $(^NameDA) zein karpetatik ezabatuko den."
     
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Ezabatzen"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Mesedez itxoin $(^NameDA) ezabatzen den bitartean."
  
  !define MUI_UNTEXT_FINISH_TITLE "Ezabatzea burututa"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Ezabatzea zuzen burutu da."

  !define MUI_UNTEXT_ABORT_TITLE "Ezabatzea ezeztatuta"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Ezabatzea ez da zuzen burutu."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "$(^NameDA)-ren ezabaketa laguntzailea osatzen"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) zure sistematik ezabatu da.\r\n\r\nSakatu Amaitu laguntzaile hau ixteko."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Zure ordenagailuak berrabiarazia izan behar du $(^NameDA)-ren ezabaketa osatzeko. Orain Berrabiarazi nahi duzu?"
  !define MUI_UNTEXT_ABORTWARNING "Ziur zaude $(^Name)-ren ezabaketa laguntzailetik irten nahi duzula?"

!insertmacro MUI_LANGUAGEFILE_END