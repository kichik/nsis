;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Polish (1045)
;By Piotr Murawski & Rafa³ Lampe; www.lomsel.prv.pl mailto:ppiter@skrzynka.pl
;Updates and correction since 2.0 a7 by cube cube(at)lp.net.pl
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "POLISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Polski" ;Name of the language in the language itself
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Witamy w kreatorze instalacji programu $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ten kreator pomo¿e Ci zainstalowaæ program $(^Name).\r\n\r\nZalecamy, byœ zamkn¹³ wszystkie uruchomione programy zanim rozpoczniesz instalacjê. To pozwoli na uaktualnienie niezbêdnych plików systemowych bez koniecznoœci ponownego uruchomienia komputera.\r\n\r\n"
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Umowa licencyjna"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Przed instalacj¹ programu $(^Name) zapoznaj siê z warunkami licencji."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Naciœnij klawisz Page Down, aby zobaczyæ resztê umowy."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Je¿eli akceptujesz warunki umowy, wybierz Zgadzam siê, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Je¿eli akceptujesz warunki umowy, zaznacz pole poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^Name).  $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Je¿eli akceptujesz warunki umowy, wybierz pierwsz¹ opcjê poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^Name).  $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Wybór komponentów"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Wybierz komponenty programu $(^Name), które chcesz zainstalowaæ."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Przesuñ kursor myszy nad komponent, aby zobaczyæ jego opis."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Wybór miejsca instalacji"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Wybierz folder, w którym ma byæ zainstalowany $(^Name)."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Instaluje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Proszê czekaæ, kiedy $(^Name) jest instalowany."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Zakoñczono"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Instalacja zakoñczona powodzeniem." 
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Instalacja przerwana"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Instalacja nie zosta³a zakoñczona pomyœlnie."
   
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Koniec"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Koñczenie pracy kreatora instalacji $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "$(^Name) zosta³ pomyœlnie zainstalowany na Twoim komputerze.\r\n\r\nKliknij Koniec aby zamkn¹æ kreatora."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Twój komputer musi zostaæ ponownie uruchomiony aby zakoñczyæ instalacjê programu $(^Name). Czy chcesz zrobiæ to teraz?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Uruchom ponownie teraz"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "PóŸniej sam uruchomiê komputer ponownie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Uruchom program $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Poka¿ plik czytajto"  
 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Wybierz folder w menu Start"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Wybierz folder menu Start w którym zostan¹ umieszczone skróty do programu"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Wybierz folder w menu Start w którym chcia³byœ umieœciæ skróty do programu. Mo¿esz tak¿e utworzyæ nowy folder wpisuj¹c jego nazwê."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nie twórz skrótów"
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Czy na pewno chcesz wyjœæ z instalatora $(^Name)?"

    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Odinstaluj $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Usuñ $(^Name) z twojego systemu."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_TITLE "Umowa licencyjna."  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_SUBTITLE "Przed deinstalacj¹ programu $(^Name) zapoznaj siê z warunkami licencji."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM "Je¿eli akceptujesz warunki umowy, wybierz Zgadzam siê, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Je¿eli akceptujesz warunki umowy, zaznacz pole poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^Name). $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Je¿eli akceptujesz warunki umowy, wybierz pierwsz¹ opcjê poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^Name). $_CLICK"
      
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Wybierz komponenty"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Wybierz, które elementy $(^Name) chcesz odinstalowaæ."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_TITLE "Wybór miejsca deinstalacji"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_SUBTITLE "Wybierz folder, z którego chcesz odinstalowaæ $(^Name)."  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Odinstalowuje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Proszê czekaæ, $(^Name) jest odinstalowywany."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Zakoñczono odinstalowanie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Odinstalowanie zakoñczone pomyœlnie."
   
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Odinstalowanie przerwane"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Odinstalowanie nie zosta³o zakoñczone pomyœlnie."
     
!insertmacro MUI_LANGUAGEFILE_END