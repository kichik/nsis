;NSIS Modern User Interface - Turkish Language File
;Compatible with Modern UI 1.65

;Language: Turkish (1055)
;By Fatih BOY (fatih@smartcoding.org)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "TURKISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Turkish";

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Devam'a tiklayin."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Kurulumu baslatmak için KUR'a tiklayiniz."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "${MUI_PRODUCT} Kurulum Sihirbazina hosgeldiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Bu sihirbaz size ${MUI_PRODUCT} kurulumu boyunca rehberlik edecektir.\r\n\r\nKurulumu baslatmadan önce çalisan diger programlari kapatmanizi öneririz. Böylece bilgisayarinizi yeniden baslatmadan bazi sistem dosyalari sorunsuz kurulabilir.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Sözlesme"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Lütfen ${MUI_PRODUCT} programini kurmadan önce sözlesmeyi okuyunuz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Sözlesmenin gire kalanini görmek için Page Down'a basabilirsiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "If you accept the terms of the agreement, click I Agree to continue. You must accept the agreement to install ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Sözlesmeyi kabul ediyorsaniz 'Kabul'e basin. ${MUI_PRODUCT} programini kurabilmeniz için sözlesmeyi kabul etmeniz gerekli."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Sözlesmeyi kabul ediyorsaniz, asagidaki ilk seçenegi seçiniz. ${MUI_PRODUCT} programini kurabilmeniz için sözlesmeyi kabul etmeniz gerekli."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Paket Seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Lütfen ${MUI_PRODUCT} için kurmak istediginiz paketleri seçin."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Kurulmasini istediginiz paketleri isaretleyiniz, kurulmasini istemediginiz paketlerin ise isaretlerini kaldiriniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Açiklama"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Paketlerin açiklamalarini görmek için mouse ile üzerine gelin."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Hedef dizini seçin"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "${MUI_PRODUCT} programini kurmak istediginiz dizini seçin."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} asagida belirttiginiz dizine kurulacak.$\r$\n$\r$\nFarkli bir dizine kurmak istiyorsaniz, 'Farkli Dizin'e tiklayin."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Hedef Dizin"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Kuruluyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Lütfen ${MUI_PRODUCT} kurulurken bekleyiniz"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Kurulum Tamamlandi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Kurulum basariyla gerçeklesti."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Kurulum Iptal Edildi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Kurulum basariyla gerçeklestirilemedi."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Bitir"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "${MUI_PRODUCT} Kurulumu Tamamlaniyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} basariyla bilgisayariniza yüklendi.\r\n\r\nLütfen 'Bitir'e basarak kurulumu sonlandirin."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "${MUI_PRODUCT} kurulumunun tamamlanmasi için bilgisayarinizi yeniden baslatmaniz gerekli. Bunu simdi yapmak istiyor musunuz?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Yeniden baslat"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Bilgisayari daha sonra elle tekrar baslatacagim"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "${MUI_PRODUCT} programini &çalistir"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "'Beni Oku' dosyasini &göster"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Baslat menüsü klasörü seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Programin kisayollarinin konulacagi baslat menüsü klasörünü seçiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Programin kisayollarinin konulacagi baslat menüsü klasörünü seçiniz. Ayni zamanda farkli bir isim girerek yeni bir klasör yaratabilirsiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Kisa yollari olusturma"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "${MUI_PRODUCT} kurulumundan çikmak istediginize emin misiniz?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "'Kaldir'a basarak kaldirma islemini baslatabilirsiniz."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "${MUI_PRODUCT} programini kaldir"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "${MUI_PRODUCT} programini bilgisayarinizdan kaldirma."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "This wizard will uninstall ${MUI_PRODUCT} from your computer."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Kaldiriliyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Lütfen ${MUI_PRODUCT} kaldirilirken bekleyiniz."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Kaldirma tamamlandi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Program basariyla kaldirildi."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Program kaldirma iptal edildi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Program kaldirilma islemi basarisiz oldu."
  
!insertmacro MUI_LANGUAGEFILE_END