;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: Polish (1045)
;By Piotr Murawski & Rafa³ Lampe; www.lomsel.prv.pl mailto:ppiter@skrzynka.pl
;Updates and correction since 2.0 a7 by cube cube(at)lp.net.pl
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "POLISH"

  !define MUI_LANGNAME "Polski" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Witamy w kreatorze instalacji programu $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ten kreator pomo¿e Ci zainstalowaæ program $(^Name).\r\n\r\nZalecamy, byœ zamkn¹³ wszystkie uruchomione programy zanim rozpoczniesz instalacjê. To pozwoli na uaktualnienie niezbêdnych plików systemowych bez koniecznoœci ponownego uruchomienia komputera.\r\n\r\n"
    
  !define MUI_TEXT_LICENSE_TITLE "Umowa licencyjna"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Przed instalacj¹ programu $(^Name) zapoznaj siê z warunkami licencji."
  !define MUI_INNERTEXT_LICENSE_TOP "Naciœnij klawisz Page Down, aby zobaczyæ resztê umowy."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Je¿eli akceptujesz warunki umowy, wybierz Zgadzam siê, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Je¿eli akceptujesz warunki umowy, zaznacz pole poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^Name).  $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Je¿eli akceptujesz warunki umowy, wybierz pierwsz¹ opcjê poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^Name).  $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Wybór komponentów"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Wybierz komponenty programu $(^Name), które chcesz zainstalowaæ."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Przesuñ kursor myszy nad komponent, aby zobaczyæ jego opis."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Wybór miejsca instalacji"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Wybierz folder, w którym ma byæ zainstalowany $(^Name)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Instaluje"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Proszê czekaæ, kiedy $(^Name) jest instalowany."
  
  !define MUI_TEXT_FINISH_TITLE "Zakoñczono"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalacja zakoñczona powodzeniem." 
  
  !define MUI_TEXT_ABORT_TITLE "Instalacja przerwana"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalacja nie zosta³a zakoñczona pomyœlnie."
   
  !define MUI_BUTTONTEXT_FINISH "&Koniec"
  !define MUI_TEXT_FINISH_INFO_TITLE "Koñczenie pracy kreatora instalacji $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) zosta³ pomyœlnie zainstalowany na Twoim komputerze.\r\n\r\nKliknij Koniec aby zamkn¹æ kreatora."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Twój komputer musi zostaæ ponownie uruchomiony aby zakoñczyæ instalacjê programu $(^Name). Czy chcesz zrobiæ to teraz?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Uruchom ponownie teraz"
  !define MUI_TEXT_FINISH_REBOOTLATER "PóŸniej sam uruchomiê komputer ponownie"
  !define MUI_TEXT_FINISH_RUN "Uruchom program $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Poka¿ plik czytajto"  
 
  !define MUI_TEXT_STARTMENU_TITLE "Wybierz folder w menu Start"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Wybierz folder menu Start w którym zostan¹ umieszczone skróty do programu"
  !define MUI_INNERTEXT_STARTMENU_TOP "Wybierz folder w menu Start w którym chcia³byœ umieœciæ skróty do programu. Mo¿esz tak¿e utworzyæ nowy folder wpisuj¹c jego nazwê."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nie twórz skrótów"
    
  !define MUI_TEXT_ABORTWARNING "Czy na pewno chcesz wyjœæ z instalatora $(^Name)?"

    
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Witamy w Kreatorze Odinstalowania $(^Name)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Kreator poprowadzi Ciê przez proces odinstalowania (usuniêcia z dysku) $(^Name).\r\n\r\nPrzed rozpoczêciem odinstalowania programu, upewnij siê, i¿ $(^Name) NIE jest w³aœnie uruchomiony.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Odinstaluj $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Usuñ $(^Name) z twojego systemu."

  !define MUI_UNTEXT_LICENSE_TITLE "Umowa licencyjna."  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Przed deinstalacj¹ programu $(^Name) zapoznaj siê z warunkami licencji."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Je¿eli akceptujesz warunki umowy, wybierz Zgadzam siê, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Je¿eli akceptujesz warunki umowy, zaznacz pole poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Je¿eli akceptujesz warunki umowy, wybierz pierwsz¹ opcjê poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^Name). $_CLICK"
      
  !define MUI_UNTEXT_COMPONENTS_TITLE "Wybierz komponenty"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Wybierz, które elementy $(^Name) chcesz odinstalowaæ."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Wybór miejsca deinstalacji"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Wybierz folder, z którego chcesz odinstalowaæ $(^Name)."  
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Odinstalowuje"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Proszê czekaæ, $(^Name) jest odinstalowywany."
  
  !define MUI_UNTEXT_FINISH_TITLE "Zakoñczono odinstalowanie"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Odinstalowanie zakoñczone pomyœlnie."
   
  !define MUI_UNTEXT_ABORT_TITLE "Odinstalowanie przerwane"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Odinstalowanie nie zosta³o zakoñczone pomyœlnie."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Koñczenie pracy Kreatora Odinstalowania $(^Name)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) zosta³ odinstalowany z dysku Twojego komputera.\r\n\r\nKliknij na przycisku „Finish” (Zakoñcz), aby zakoñczyæ dzia³nie Kreatora."
  
  !define MUI_UNTEXT_ABORTWARNING "Czy na pewno chcesz zakoñczyæ proces odinstalowywania $(^Name)?"
     
!insertmacro MUI_LANGUAGEFILE_END