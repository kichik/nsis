;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4 and Basic Modern UI 1.61 scripts (needs te be updated!)

;Language: Polish (1045)
;By Piotr Murawski & Rafa³ Lampe; www.lomsel.prv.pl mailto:ppiter@skrzynka.pl

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "POLISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Polski" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Umowa licencyjna"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Proszê przejrzy warunki licencji przed instalacj¹ ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Proszê naduœ klawisz Page Down, aby zobaczyæ resztê umowy."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Je¿eli akceptujesz wszystkie warunki umnowy wybierz Zgadzam siê, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Wybór komponentów"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Wybierz komponenty ${MUI_PRODUCT}, które chcesz zainstalowaæ."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Zaznacz komponenty, które chcesz zainstalowaæ oraz odznacz komponenty, które nie maj¹ byæ zainstalowane. Kliknij przycisk Dalej, aby kontynuowaæ."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Przesuñ kursor myszy nad komponent, aby zobaczyæ jego opis."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Wybór miejsca instalacji"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Wybierz folder, w którym ma byæ zainstalowany ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Instalator zainstaluje ${MUI_PRODUCT} do nastêpuj¹cego folderu.$\r$\n$\r$\nAby zainstalowaæ w tym folderze kliknij Instaluj. Aby zainstalowaæ w innym folderze kliknij Przegl¹daj i wybierz inny folder."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Folder docelowy"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Instaluje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Proszê czekaæ, kiedy ${MUI_PRODUCT} jest instalowany."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Zakoñczono"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Instalacja zakoñczona powodzeniem."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Czy na pewno chcesz wyjœæ z instalatora ${MUI_PRODUCT}?"
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Odinstaluj ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Usuñ ${MUI_PRODUCT} z twojego systemu."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Deinstalator odinstaluje ${MUI_PRODUCT} z twojego systemu."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Odinstalowuje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Proszê czekaæ, kiedy ${MUI_PRODUCT} jest odinstalowywany."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Zakoñczono"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Odinstalowanie zakoñczone powodzeniem."
    
!insertmacro MUI_LANGUAGEFILE_END