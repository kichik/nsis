;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Hungarian (1038)
;Translation by Jozsef Tamas Herczeg ( - 1.61-ig),
;               Lajos Molnar (Orfanik) <orfanik@axelero.hu> ( 1.62 - tõl)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "HUNGARIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Magyar" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Üdvözli a(z) $(^Name) Telepítõ Varázsló"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "A(z) $(^Name) telepítése következik a számítógépre.\r\n\r\nJavasoljuk, hogy indítás elõtt zárja be a futó alkalmazásokat. Így a telepítõ a rendszer újraindítása nélkül tudja frissíteni a szükséges rendszerfájlokat.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licencszerzõdés"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "A(z) $(^Name) telepítése elõtt tekintse át a szerzõdés feltételeit."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "A PageDown gombbal olvashatja el a szerzõdés folytatását."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ha elfogadja a szerzõdés valamennyi feltételét, az Elfogadom gombbal folytathatja. El kell fogadnia a(z) $(^Name) telepítéséhez."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Amennyiben elfogadja a feltételeket, jelölje be a jelölõnényzeten. A(z) $(^Name) telepítéséhez el kell fogadnia a feltételeket. $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Amennyiben elfogadja a feltételeket, válassza az elsõ opciót. A(z) $(^Name) telepítéséhez el kell fogadnia a feltételeket. $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Összetevõk kiválasztása"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Válassza ki, hogy a(z) $(^Name) mely funkcióit kívánja telepíteni."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Leírás"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Vigye rá az egeret az összetevõre, hogy megtekinthesse a leírását."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Telepítési hely kiválasztása"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Válassza ki a(z) $(^Name) telepítésének mappáját."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Telepítési folyamat"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Kis türelmet a(z) $(^Name) telepítéséig."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Telepítés befejezõdött"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "A telepítés sikeresen befejezõdött."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "A telepítés megszakadt"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "A telepítés sikertelen volt."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Befejezés"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "A(z) $(^Name) telepítése megtörtént."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "A(z) $(^Name) telepítése megtörtént.\r\n\r\nA Befejezés gomb megnyomásával zárja be a varázslót."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "A(z) $(^Name) telepítésének befejezéséhez újra kell indítani a rendszert. Most akarja újraindítani?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Most indítom újra"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Késõbb fogom újraindítani"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "$(^Name) futtatása"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "OlvassEl fájl megjelenítése"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Start menü mappa kijelölése"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Start menü mappa kijelölése a program parancsikonjaihoz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Jelöljön ki egy mappát a Start menüben, melybe a program parancsikonjait fogja elhelyezni. Beírhatja új mappa nevét is."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nincs parancsikon elhelyezés"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Biztos, hogy ki akar lépni a(z) $(^Name) Telepítõbõl?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "A(z) $(^Name) Eltávolítása."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "A(z) $(^Name) eltávolítása következik a számítógéprõl."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_TITLE "Licencszerzõdés"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_SUBTITLE "A(z) $(^Name) eltávolítása elõtt tekintse át a szerzõdés feltételeit."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM "Ha elfogadja a szerzõdés valamennyi feltételét, az Elfogadom gombbal folytathatja. El kell fogadnia a(z) $(^Name) eltávolításához."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Amennyiben elfogadja a feltételeket, jelölje be a jelölõnényzeten. A(z) $(^Name) eltávolításához el kell fogadnia a feltételeket. $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Amennyiben elfogadja a feltételeket, válassza az elsõ opciót. A(z) $(^Name) eltávolításához el kell fogadnia a feltételeket. $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Összetevõk kiválasztása"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Válassza ki, hogy a(z) $(^Name) mely funkcióit kívánja eltávolítani."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_TITLE "Telepítési hely kiválasztása"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_SUBTITLE "Válassza ki a(z) $(^Name) telepítésének mappáját."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Eltávolítási folyamat"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Kis türelmet a(z) $(^Name) eltávolításáig."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Az eltávolítás befejezõdött"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Az eltávolítás sikeresen befejezõdött."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Az eltávolítás megszakadt"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Az eltávolítás sikertelen volt."

!insertmacro MUI_LANGUAGEFILE_END


