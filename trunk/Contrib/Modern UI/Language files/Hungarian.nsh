;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.63

;Language: Hungarian (1038)
;Translation by Jozsef Tamas Herczeg, Lajos Molnar (Orfanik) <orfanik@axelero.hu>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "HUNGARIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Magyar" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "A folytatáshoz nyomja meg a Tovább gombot."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "A Telepítés gomb megnyomásával indíthatja a folyamatot."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Üdvözli a(z) ${MUI_PRODUCT} Telepítõ Varázsló"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "A(z) ${MUI_PRODUCT} telepítése következik a számítógépre.\r\n\r\nJavasoljuk, hogy indítás elõtt zárja be a futó alkalmazásokat. Így a telepítõ a rendszer újraindítása nélkül tudja frissíteni a szükséges rendszerfájlokat.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licencszerzõdés"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "A(z) ${MUI_PRODUCT} telepítése elõtt tekintse át a szerzõdés feltételeit."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "A PageDown gombbal olvashatja el a szerzõdés folytatását."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ha elfogadja a szerzõdés valamennyi feltételét, az Elfogadom gombbal folytathatja. El kell fogadnia a(z) ${MUI_PRODUCT} telepítéséhez."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Összetevõk kiválasztása"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Válassza ki, hogy ${MUI_PRODUCT} mely funkcióit kívánja telepíteni."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Jelölje be a telepíteni kívánt összetevõket, és törölje a telepíteni nem kívánt elemeket."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Leírás"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Vigye rá az egeret az összetevõre, hogy megtekinthesse a leírását."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Telepítési hely kiválasztása"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Válassza ki a(z) ${MUI_PRODUCT} telepítésének mappáját."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "A(z) ${MUI_PRODUCT} telepítése a következõ mappába történik.$\r$\n$\r$\nEltérõ mappába történõ telepítéshez a Tallózás gombbal jelölhet ki másik mappát."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Célmappa"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Telepítési folyamat"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Kis türelmet a(z) ${MUI_PRODUCT} telepítéséig."
 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Elkészült telepítés"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "A telepítés sikeresen befejezõdött."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Befejezés"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "A(z) ${MUI_PRODUCT} telepítése megtörtént."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "A(z) ${MUI_PRODUCT} telepítése megtörtént.\r\n\r\nA Befejezés gomb megnyomásával zárja be a varázslót."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "A(z) ${MUI_PRODUCT} telepítésének befejezéséhez újra kell indítani a rendszert. Most akarja újraindítani?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Most indítom újra"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Késõbb fogom újraindítani"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "${MUI_PRODUCT} futtatása"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "OlvassEl fájl megjelenítése"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Start menü mappa kijelölése"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Start menü mappa kijelölése a program parancsikonjaihoz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Jelöljön ki egy mappát a Start menüben, melybe a program parancsikonjait fogja elhelyezni. Beírhatja új mappa nevét is."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nincs parancsikon elhelyezés"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Biztos, hogy ki akar lépni a(z) ${MUI_PRODUCT} Telepítõbõl?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Az ltávolításhoz nyomja meg az Eltávolítás gombot."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Eltávolítás: ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "A(z) ${MUI_PRODUCT} eltávolítása a rendszerbõl."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "A(z) ${MUI_PRODUCT} programot távolítja el a rendszerbõl."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Eltávolítási folyamat"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Kis türelmet a(z) ${MUI_PRODUCT} eltávolításáig."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Befejezés"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Az eltávolítás sikeresen befejezõdött."
  
!insertmacro MUI_LANGUAGEFILE_END