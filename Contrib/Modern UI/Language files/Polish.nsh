;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Polish (1045)
;By Piotr Murawski & Rafa³ Lampe; www.lomsel.prv.pl mailto:ppiter@skrzynka.pl
;Updated by cube, cube(at)lp.net.pl and SYSTEMsoft Group, http://www.systemsoft-group.com
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Polish"

  !define MUI_LANGNAME "Polski" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Witamy w kreatorze instalacyjny programu $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ten kreator pomo¿e Ci zainstalowaæ program $(^NameDA).\r\n\r\nZalecane jest zamkniêcie wszystkich uruchomionych programów przed rozpoczniêciem instalacji. To pozwoli na uaktualnienie niezbêdnych plików systemowych bez koniecznoœci ponownego uruchomienia komputera.\r\n\r\n$_CLICK"
    
  !define MUI_TEXT_LICENSE_TITLE "Umowa licencyjna"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Przed instalacj¹ programu $(^NameDA) zapoznaj siê z warunkami licencji."
  !define MUI_INNERTEXT_LICENSE_TOP "Naciœnij klawisz Page Down, aby zobaczyæ resztê umowy."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Je¿eli akceptujesz warunki umowy, wybierz Zgadzam siê, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Je¿eli akceptujesz warunki umowy, zaznacz pole wyboru poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^NameDA).  $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Je¿eli akceptujesz warunki umowy, wybierz pierwsz¹ opcjê poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby zainstalowaæ $(^NameDA).  $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Wybierz komponenty"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Wybierz komponenty programu $(^NameDA), które chcesz zainstalowaæ."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Przesuñ kursor myszy nad komponent, aby zobaczyæ jego opis."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Wybierz lokalizacjê dla instalacji"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Wybierz folder, w którym ma byæ zainstalowany $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Instalowanie"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Proszê czekaæ, podczas gdy $(^NameDA) jest instalowany."
  
  !define MUI_TEXT_FINISH_TITLE "Zakoñczono"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalacja zakoñczona pomyœlnie."
  
  !define MUI_TEXT_ABORT_TITLE "Instalacja przerwana"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalacja nie zosta³a zakoñczona pomyœlnie."
   
  !define MUI_BUTTONTEXT_FINISH "&Zakoñcz"
  !define MUI_TEXT_FINISH_INFO_TITLE "Koñczenie pracy kreatora instalacji $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) zosta³ pomyœlnie zainstalowany na Twoim komputerze.\r\n\r\nKliknij Zakoñcz, aby zakoñczyæ dzia³nie Kreatora."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Twój komputer musi zostaæ ponownie uruchomiony aby zakoñczyæ instalacjê programu $(^NameDA). Czy chcesz zrobiæ to teraz?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Uruchom ponownie teraz"
  !define MUI_TEXT_FINISH_REBOOTLATER "Sam uruchomiê ponownie komputer póŸniej"
  !define MUI_TEXT_FINISH_RUN "Uruchom program $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Poka¿ plik ReadMe"
 
  !define MUI_TEXT_STARTMENU_TITLE "Wybierz folder w menu Start"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Wybierz folder menu Start w którym zostan¹ umieszczone skróty do programu"
  !define MUI_INNERTEXT_STARTMENU_TOP "Wybierz folder w menu Start w którym chcia³byœ umieœciæ skróty do programu. Mo¿esz tak¿e utworzyæ nowy folder wpisuj¹c jego nazwê."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nie twórz skrótów"
    
  !define MUI_TEXT_ABORTWARNING "Czy na pewno chcesz zakoñczyæ dzia³anie instalatora $(^Name)?"

    
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Witamy w Kreatorze Deinstalacji $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Kreator poprowadzi Ciê przez proces deinstalacji $(^NameDA).\r\n\r\nPrzed rozpoczêciem deinstalacji programu, upewnij siê, czy $(^NameDA) NIE jest w³aœnie uruchomiony.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Odinstaluj $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Usuñ $(^NameDA) z twojego komputera."

  !define MUI_UNTEXT_LICENSE_TITLE "Umowa Licencyjna."
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Przed deinstalacj¹ programu $(^NameDA) zapoznaj siê z warunkami licencji."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Je¿eli akceptujesz warunki umowy, wybierz Zgadzam siê, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Je¿eli akceptujesz warunki umowy, zaznacz pole wyboru poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Je¿eli akceptujesz warunki umowy, wybierz pierwsz¹ opcjê poni¿ej, aby kontynuowaæ. Musisz zaakceptowaæ warunki umowy, aby odinstalowaæ $(^NameDA). $_CLICK"
      
  !define MUI_UNTEXT_COMPONENTS_TITLE "Wybierz komponenty"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Wybierz, które elementy $(^NameDA) chcesz odinstalowaæ."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Wybór miejsca deinstalacji"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Wybierz folder, z którego chcesz odinstalowaæ $(^NameDA)."  
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Deinstalowanie"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Proszê czekaæ, $(^NameDA) jest odinstalowywany."
  
  !define MUI_UNTEXT_FINISH_TITLE "Zakoñczono odinstalowanie"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Odinstalowanie zakoñczone pomyœlnie."
   
  !define MUI_UNTEXT_ABORT_TITLE "Deinstalacja przerwana"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Deinstalacja nie zosta³a zakoñczona pomyœlnie."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Koñczenie pracy Kreatora Deinstalacyjnego $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) zosta³ odinstalowany z Twojego komputera.\r\n\r\nKliknij Zakoñcz, aby zakoñczyæ dzia³nie Kreatora."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Twój komputer musi zostaæ ponownie uruchomiony w celu zakoñczenia deinstalacji programu $(^NameDA). Czy chcesz zrobiæ to teraz?"
  
  !define MUI_UNTEXT_ABORTWARNING "Czy na pewno chcesz przerwaæ proces deinstalacji $(^Name)?"
     
!insertmacro MUI_LANGUAGEFILE_END