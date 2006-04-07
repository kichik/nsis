;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Welsh (1106)
;By Rhoslyn Prys, Meddal.com

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Welsh"

  !define MUI_LANGNAME "Welsh" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Croeso i Ddewin Gosod $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Bydd y dewin yn eich arwain drwy osodiad $(^NameDA).\r\n\r\nCaewch pob rhaglen cyn cychwyn y rhaglen osod. Bydd hyn yn ei gwneud yn bosibl i ddiweddaru'r ffeiliau system berthnasol heb fod angen ailgychwyn eich cyfrifiadur.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Cytundeb Trwyddedu"
  !define MUI_TEXT_LICENSE_SUBTITLE "Darllenwch amodau'r drwydded cyn gosod $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Pwyswch Page Down i ddarllen gweddill y cytundeb."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Os ydych yn derbyn amodau'r cytundeb, cliciwch Cytuno i barhau. Mae'n rhaid i chi dderbyn amodau'r cytundeb er mwyn gosod $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Os ydych yn derbyn amodau'r cytundeb, cliciwch y blwch ticio isod. Mae'n rhaid i chi dderbyn amodau'r cytundeb er mwyn gosod $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Os ydych yn derbyn amodau'r cytundeb, cliciwch y dewis cyntaf isod. Mae'n rhaid i chi dderbyn amodau'r cytundeb er mwyn gosod $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Dewis Cydrannau"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Dewis pa nodweddion o $(^NameDA) rydych am eu gosod."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Disgrifiad"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Gosod eich llygoden dros gydran i weld ei ddisgrifiad."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Dewis cydran i weld ei ddisgrifiad."
  !endif

  !define MUI_TEXT_DIRECTORY_TITLE "Dewis Lleoliad Gosod"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Dewis y ffolder i osod $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Gosod"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Arhoswch tra fo $(^NameDA) yn cael ei osod."

  !define MUI_TEXT_FINISH_TITLE "Cwblhawyd y Gosod"
  !define MUI_TEXT_FINISH_SUBTITLE "Mae'r Gosod wedi ei gwblhau'n llwyddiannus."

  !define MUI_TEXT_ABORT_TITLE "Ataliwyd y Gosod"
  !define MUI_TEXT_ABORT_SUBTITLE "Methwyd â chwblhau'r gosod yn llwyddiannus."

  !define MUI_BUTTONTEXT_FINISH "&Gorffen"
  !define MUI_TEXT_FINISH_INFO_TITLE "Cwblhau Dewin Gosod $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Mae $(^NameDA) wedi cael ei osod ar eich cyfrifiadur.\r\n\r\nCliciwch Gorffen i gau'r dewin."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Rhaid ailgychwyn eich cyfrifiadur i gwblhau gosod $(^NameDA). Hoffech chi ailgychwyn?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Ailgychwyn"
  !define MUI_TEXT_FINISH_REBOOTLATER "Rwyf am ailgychwyn yn hwyrach"
  !define MUI_TEXT_FINISH_RUN "&Rhedeg $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Dangos Darllenfi"

  !define MUI_TEXT_STARTMENU_TITLE "Dewis Ffolder Dewislen Cychwyn"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Dewis ffolder Dewislen Cychwyn ar gyfer llwybrau byr $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Dewis ffolder Dewislen Cychwyn i greu llwybrau byr y rhaglen. Gallwch roi enw i greu ffolder newydd."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Peidio creu llwybrau byr"

  !define MUI_TEXT_ABORTWARNING "Ydych chi'n si?r eich bod am adael Rhaglen Osod $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Croeso i Ddewin Dadosod $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Bydd y dewin yn eich arwain drwy ddadosod $(^NameDA).\r\n\r\nCyn cychwyn dadosod, gwnewch yn siwr nad yw $(^NameDA) yn rhedeg.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Dadosod $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Tynnu $(^NameDA) oddiar eich cyfrifiadur."

  !define MUI_UNTEXT_LICENSE_TITLE "Cytundeb Trwyddedu"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Darllenwch amodau'r drwydded cyn dadosod $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Os ydych yn derbyn amodau'r cytundeb, cliciwch Cytuno i barhau. Mae'n rhaid i chi dderbyn amodau'r cytundeb er mwyn dadosod  $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Os ydych yn derbyn amodau'r cytundeb, cliciwch y blwch ticio isod. Mae'n rhaid i chi dderbyn amodau'r cytundeb er mwyn dadosod $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Os ydych yn derbyn amodau'r cytundeb, cliciwch y dewis cyntaf isod. Mae'n rhaid i chi dderbyn amodau'r cytundeb er mwyn dadosod $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Dewis Cydrannau"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Dewis pa nodweddion o $(^NameDA) i'w dadoso."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Dewis Lleoliad Dadosod"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Dewis y ffolder i ddadosod $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Dadosod"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Arhoswch tra bo $(^NameDA) yn cael ei ddadosod."

  !define MUI_UNTEXT_FINISH_TITLE "Cwblhawyd y Dadosod"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Mae'r Dadosod wedi ei gwblhau'n llwyddiannus."

  !define MUI_UNTEXT_ABORT_TITLE "Ataliwyd y Dadosod"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Methwyd â chwblhau'r dadosod yn llwyddiannus."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Cwblhau Dewin Dadosod $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Mae $(^NameDA) wedi ei ddadosod oddi ar eich cyfrifiadur.\r\n\r\nCliciwch Gorffen i gau'r dewin."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Rhaid ailgychwyn eich cyfrifiadur i gwblhau gosod $(^NameDA). Hoffech chi ailgychwyn?"

  !define MUI_UNTEXT_ABORTWARNING "Ydych chi'n siwr eich bod am adael Rhaglen Dadosod $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END
