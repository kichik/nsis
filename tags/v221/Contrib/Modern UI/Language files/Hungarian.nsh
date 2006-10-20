;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Hungarian (1038)
;Translation by Jozsef Tamas Herczeg ( - 1.61-ig),
;               Lajos Molnar (Orfanik) <orfanik@axelero.hu> ( 1.62 - tõl)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Hungarian"

  !define MUI_LANGNAME "Magyar" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Üdvözli a(z) $(^NameDA) Telepítõ Varázsló"
  !define MUI_TEXT_WELCOME_INFO_TEXT "A(z) $(^NameDA) telepítése következik a számítógépre.\r\n\r\nJavasoljuk, hogy indítás elõtt zárja be a futó alkalmazásokat. Így a telepítõ a rendszer újraindítása nélkül tudja frissíteni a szükséges rendszerfájlokat.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licencszerzõdés"
  !define MUI_TEXT_LICENSE_SUBTITLE "A(z) $(^NameDA) telepítése elõtt tekintse át a szerzõdés feltételeit."
  !define MUI_INNERTEXT_LICENSE_TOP "A PageDown gombbal olvashatja el a szerzõdés folytatását."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ha elfogadja a szerzõdés valamennyi feltételét, az Elfogadom gombbal folytathatja. El kell fogadnia a(z) $(^NameDA) telepítéséhez."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Amennyiben elfogadja a feltételeket, jelölje be a jelölõnényzeten. A(z) $(^NameDA) telepítéséhez el kell fogadnia a feltételeket. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Amennyiben elfogadja a feltételeket, válassza az elsõ opciót. A(z) $(^NameDA) telepítéséhez el kell fogadnia a feltételeket. $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Összetevõk kiválasztása"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Válassza ki, hogy a(z) $(^NameDA) mely funkcióit kívánja telepíteni."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Leírás"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Vigye rá az egeret az összetevõre, hogy megtekinthesse a leírását."

  !define MUI_TEXT_DIRECTORY_TITLE "Telepítési hely kiválasztása"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Válassza ki a(z) $(^NameDA) telepítésének mappáját."

  !define MUI_TEXT_INSTALLING_TITLE "Telepítési folyamat"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Kis türelmet a(z) $(^NameDA) telepítéséig."

  !define MUI_TEXT_FINISH_TITLE "Telepítés befejezõdött"
  !define MUI_TEXT_FINISH_SUBTITLE "A telepítés sikeresen befejezõdött."

  !define MUI_TEXT_ABORT_TITLE "A telepítés megszakadt"
  !define MUI_TEXT_ABORT_SUBTITLE "A telepítés sikertelen volt."

  !define MUI_BUTTONTEXT_FINISH "&Befejezés"
  !define MUI_TEXT_FINISH_INFO_TITLE "A(z) $(^NameDA) telepítése megtörtént."
  !define MUI_TEXT_FINISH_INFO_TEXT "A(z) $(^NameDA) telepítése megtörtént.\r\n\r\nA Befejezés gomb megnyomásával zárja be a varázslót."
  !define MUI_TEXT_FINISH_INFO_REBOOT "A(z) $(^NameDA) telepítésének befejezéséhez újra kell indítani a rendszert. Most akarja újraindítani?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Most indítom újra"
  !define MUI_TEXT_FINISH_REBOOTLATER "Késõbb fogom újraindítani"
  !define MUI_TEXT_FINISH_RUN "$(^NameDA) futtatása"
  !define MUI_TEXT_FINISH_SHOWREADME "OlvassEl fájl megjelenítése"

  !define MUI_TEXT_STARTMENU_TITLE "Start menü mappa kijelölése"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Start menü mappa kijelölése a program parancsikonjaihoz."
  !define MUI_INNERTEXT_STARTMENU_TOP "Jelöljön ki egy mappát a Start menüben, melybe a program parancsikonjait fogja elhelyezni. Beírhatja új mappa nevét is."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nincs parancsikon elhelyezés"

  !define MUI_TEXT_ABORTWARNING "Biztos, hogy ki akar lépni a(z) $(^Name) Telepítõbõl?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Üdvözli a(z) $(^NameDA) Eltávolító Varázsló"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ez a varázsló segíti a(z) $(^NameDA) eltávolításában.\r\n\r\nMielõtt elkezdi az eltávilítást gyõzõdjön meg arról, hogy a(z) $(^NameDA) nem fut.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "A(z) $(^NameDA) Eltávolítása."
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "A(z) $(^NameDA) eltávolítása következik a számítógéprõl."

  !define MUI_UNTEXT_LICENSE_TITLE "Licencszerzõdés"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "A(z) $(^NameDA) eltávolítása elõtt tekintse át a szerzõdés feltételeit."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ha elfogadja a szerzõdés valamennyi feltételét, az Elfogadom gombbal folytathatja. El kell fogadnia a(z) $(^NameDA) eltávolításához."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Amennyiben elfogadja a feltételeket, jelölje be a jelölõnényzeten. A(z) $(^NameDA) eltávolításához el kell fogadnia a feltételeket. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Amennyiben elfogadja a feltételeket, válassza az elsõ opciót. A(z) $(^NameDA) eltávolításához el kell fogadnia a feltételeket. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Összetevõk kiválasztása"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Válassza ki, hogy a(z) $(^NameDA) mely funkcióit kívánja eltávolítani."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Telepítési hely kiválasztása"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Válassza ki a(z) $(^NameDA) telepítésének mappáját."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Eltávolítási folyamat"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Kis türelmet a(z) $(^NameDA) eltávolításáig."

  !define MUI_UNTEXT_FINISH_TITLE "Az eltávolítás befejezõdött"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Az eltávolítás sikeresen befejezõdött."

  !define MUI_UNTEXT_ABORT_TITLE "Az eltávolítás megszakadt"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Az eltávolítás sikertelen volt."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "A(z) $(^NameDA) eltávolítás varázslójának befejezése."
  !define MUI_UNTEXT_FINISH_INFO_TEXT "A(z) $(^NameDA) eltávolítása sikeresen befejezõdött.\r\n\r\nA Finish-re kattintva bezárul ez a varázsló."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "A számítógépet újra kell indítani, hogy a(z) $(^NameDA) eltávolítása teljes legyen. Akarja most újraindítani a rendszert?"

  !define MUI_UNTEXT_ABORTWARNING "Biztos, hogy ki akar lépni a(z) $(^Name) Eltávolítóból?"

!insertmacro MUI_LANGUAGEFILE_END