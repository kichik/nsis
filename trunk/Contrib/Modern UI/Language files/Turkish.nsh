;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Turkish (1055)
;By Fatih BOY (fatih@smartcoding.org)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "TURKISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Turkish" ;Name of the language in the language itself
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "$(^Name) Kurulum Sihirbazina hosgeldiniz"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Bu sihirbaz size $(^Name) kurulumu boyunca rehberlik edecektir.\r\n\r\nKurulumu baslatmadan önce çalisan diger programlari kapatmanizi öneririz. Böylece bilgisayarinizi yeniden baslatmadan bazi sistem dosyalari sorunsuz kurulabilir.\r\n\r\n$_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Sözlesme"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Lütfen $(^Name) programini kurmadan önce sözlesmeyi okuyunuz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Sözlesmenin geri kalanini görmek için Page Down'a basabilirsiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Sözleþme koþullarýný kabul ediyorsanýz, 'Kabul Ediyorum'a basýnýz. $(^Name) programýný kurmak için sözleþme koþullarýný kabul etmelisiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Sözleþme koþullarýný kabul ediyorsanýz, aþaðýdaki check box'a basýnýz. $(^Name) programýný kurmak için sözleþme koþullarýný kabul etmelisiniz. $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Sözleþme koþullarýný kabul ediyorsanýz, asagidaki ilk seçenegi seçiniz. $(^Name) programýný kurmak için sözleþme koþullarýný kabul etmelisiniz. $_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Paket Seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Lütfen $(^Name) için kurmak istediginiz paketleri seçiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Açiklama"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Paketlerin açiklamalarini görmek için mouse ile üzerine gelin."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Hedef dizini seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "$(^Name) programini kurmak istediginiz dizini seçin."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Kuruluyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "$(^Name) kurulurken bekleyiniz."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Kurulum Tamamlandi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Kurulum basariyla gerçeklesti."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Kurulum Iptal Edildi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Kurulum basariyla gerçeklestirilemedi."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Bitir"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "$(^Name) Kurulumu Tamamlaniyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "$(^Name)  basariyla bilgisayariniza yüklendi.\r\n\r\nLütfen 'Bitir'e basarak kurulumu sonlandirin."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "$(^Name) kurulumunun tamamlanmasi için bilgisayarinizi yeniden baslatmaniz gerekli. Bunu simdi yapmak istiyor musunuz?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Yeniden baslat"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Bilgisayari daha sonra elle tekrar baslatacagim"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "$(^Name) programini &çalistir"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "'Beni Oku' dosyasini &göster"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Baslat menüsü klasörü seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "$(^Name) kisayollarinin konulacagi baslat menüsü klasörünü seçiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Programin kisayollarinin konulacagi baslat menüsü klasörünü seçiniz. Farkli bir isim girerek yeni bir klasör yaratabilirsiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Kisa yollari olusturma"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "$(^Name) kurulumundan çikmak istediginize emin misiniz?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "$(^Name) programini kaldir"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "$(^Name) programini bilgisayarinizdan kaldirma."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_TITLE "Sözlesme"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_SUBTITLE "Lütfen $(^Name) programini kaldýrmadan önce sözlesmeyi okuyunuz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM "Sözleþme koþullarýný kabul ediyorsanýz, 'Kabul Ediyorum'a basýnýz. $(^Name) programýný kaldýrmak için sözleþme koþullarýný kabul etmelisiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Sözleþme koþullarýný kabul ediyorsanýz, aþaðýdaki check box'a basýnýz. $(^Name) programýný kaldýrmak için sözleþme koþullarýný kabul etmelisiniz. $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Sözleþme koþullarýný kabul ediyorsanýz, asagidaki ilk seçenegi seçiniz. $(^Name) programýný kaldýrmak için sözleþme koþullarýný kabul etmelisiniz. $_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Paket Seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Lütfen $(^Name) için kaldýrmak istediginiz özellikleri seçiniz."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_TITLE "Kaldýrlacak Hedef Seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_SUBTITLE "$(^Name) programini kaldýrmak istediginiz dizini seçin."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Kaldýrýlýyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "$(^Name) kaldýrlýrken bekleyiniz."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Kaldýrma Tamamlandýr"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Kaldýrma basariyla gerçeklesti."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Kaldýrma Iptal Edildi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Kaldýrma basariyla gerçeklestirilemedi."
  
!insertmacro MUI_LANGUAGEFILE_END