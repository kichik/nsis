;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4

;Language: Polish (1045)
;By Piotr Murawski & Rafa³ Lampe; www.lomsel.prv.pl mailto:ppiter@skrzynka.pl

;--------------------------------
!verbose 3

!ifndef MUI_POLISH_USED

!define MUI_POLISH_USED

  !define MUI_POLISH_LANGNAME "Polski" ;Name of the language in the language itself (English, Deutsch, Français etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
    LicenseText /LANG=${LANG_POLISH} "Proszê naduœ klawisz Page Down, aby zobaczyæ resztê umowy."
    LangString MUI_TEXT_LICENSE_TITLE ${LANG_POLISH} "Umowa licencyjna"  
    LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_POLISH} "Proszê przejrzy warunki licencji przed instalacj¹ ${NAME}."
    LangString MUI_INNERTEXT_LICENSE ${LANG_POLISH} "Je¿eli akceptujesz wszystkie warunki umnowy wybierz Zgadzam siê, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ ${NAME}."
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_POLISH} "Zaznacz komponenty, które chcesz zainstalowaæ oraz odznacz komponenty, które nie maj¹ byæ zainstalowane. Kliknij przycisk Dalej, aby kontynuowaæ."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_POLISH} "Wybór komponentów"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_POLISH} "Wybierz komponenty ${NAME}, które chcesz zainstalowaæ."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_POLISH} "Opis"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_POLISH} "Przesuñ kursor myszy nad komponent, aby zobaczyæ jego opis."
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_POLISH} "Instalator zainstaluje ${NAME} do nastêpuj¹cego folderu.$\r$\n$\r$\nAby zainstalowaæ w tym folderze kliknij Instaluj. Aby zainstalowaæ w innym folderze kliknij Przegl¹daj i wybierz inny folder."
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_POLISH} "Wybór miejsca instalacji"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_POLISH} "Wybierz folder, w którym ma byæ zainstalowany ${NAME}."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_POLISH} "Folder docelowy"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_POLISH} "Instaluje"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_POLISH} "Proszê czekaæ, kiedy ${NAME} jest instalowany."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_POLISH} "Zakoñczono"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_POLISH} "Instalacja zakoñczona powodzeniem."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_POLISH} "Czy na pewno chcesz wyjœæ z instalatora ${NAME}?"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_POLISH} "${NAME} ${VERSION} Instalator"
  !endif
  
  
  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_POLISH} "Deinstalator odinstaluje ${NAME} z twojego systemu."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_POLISH} "Odinstaluj ${NAME}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_POLISH} "Usuñ ${NAME} z twojego systemu."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_POLISH} "Odinstalowuje"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_POLISH} "Proszê czekaæ, kiedy ${NAME} jest odinstalowywany."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_POLISH} "Zakoñczono"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_POLISH} "Odinstalowanie zakoñczone powodzeniem."
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_POLISH} "${NAME} ${VERSION} Instalator"
  !endif
    
!endif

!verbose 4