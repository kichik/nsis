;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: Turkish (1055)
;By Çagatay Dilsiz(Chagy)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "TURKISH"

  !define MUI_LANGNAME "Türkçe" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "$(^Name) Kurulum sihirbazýna hoþ geldiniz"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Bu sihirbaz size $(^Name) kurulumu boyunca rehberlik edecektir.\r\n\r\nKurulumu baþlatmadan önce çalýþan diðer programlari kapatmanýzý öneririz. Böylece bilgisayarýnýzý yeniden baþlatmadan bazý sistem dosyalarý sorunsuz kurulabilir.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Lisans Sözleþmesi"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Lütfen $(^Name) programýný kurmadan önce sözleþmeyi okuyunuz."
  !define MUI_INNERTEXT_LICENSE_TOP "Sözleþmenin geri kalanýný okumak için 'page down' tuþuna basabilirsiniz."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Sözleþme koþullarýný kabul ediyorsanýz, 'Kabul Ediyorum'a basýnýz. $(^Name) programýný kurmak için sözleþme koþullarýný kabul etmelisiniz."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Sözleþme koþullarýný kabul ediyorsanýz, aþaðýdaki onay kutusunu doldurunuz. $(^Name) programýný kurmak için sözleþme koþullarýný kabul etmelisiniz. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Sözleþme koþullarýný kabul ediyorsanýz, asagidaki onay düðmesini seçiniz. $(^Name) programýný kurmak için sözleþme koþullarýný kabul etmelisiniz. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Bileþen seçimi"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Lütfen $(^Name) için kurmak istediginiz bileþenleri seçiniz."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Açýklama"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Bileþenlerin açýklamalarýný görmek için imleci bileþen üzerine götürün."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Hedef dizini seçimi"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "$(^Name) programýný kurmak istediðiniz dizini þeçiniz."
  
  !define MUI_TEXT_INSTALLING_TITLE "Kuruluyor"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Lütfen $(^Name) kurulurken bekleyiniz."
  
  !define MUI_TEXT_FINISH_TITLE "Kurulum Tamamlandý"
  !define MUI_TEXT_FINISH_SUBTITLE "Kurulum baþarýyla tamamlandý."
  
  !define MUI_TEXT_ABORT_TITLE "Kurulum Ýptal Edildi"
  !define MUI_TEXT_ABORT_SUBTITLE "Kurulum tam olarak tamamlanmadý."
  
  !define MUI_BUTTONTEXT_FINISH "&Bitir"
  !define MUI_TEXT_FINISH_INFO_TITLE "$(^Name) Kurulum sihirbazý tamamlanýyor."
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name)  bilgisayariniza yüklendi.\r\n\r\nLütfen 'Bitir'e basarak kurulumu sonlandýrýn."
  !define MUI_TEXT_FINISH_INFO_REBOOT "$(^Name) kurulumunun tamamlanmasý için bilgisayarýnýzý yeniden baþlatmanýz gerekiyor.Bilgisayarýnýzý yeniden baþlatmak istiyor musunuz?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Yeniden baþlat"
  !define MUI_TEXT_FINISH_REBOOTLATER "Bilgisayarýmý daha sonra baþlatacaðým."
  !define MUI_TEXT_FINISH_RUN "$(^Name) programýný çalýþtýr"
  !define MUI_TEXT_FINISH_SHOWREADME "beni oku/readme dosyasýný &göster"
  
  !define MUI_TEXT_STARTMENU_TITLE "Baþlat Menüsü Klasör Seçimi"
  !define MUI_TEXT_STARTMENU_SUBTITLE "$(^Name) kýsayollarýnýn konulacagý baþlat menüsü klasörünü seçiniz."
  !define MUI_INNERTEXT_STARTMENU_TOP "Programýn kýsayollarýnýn konulacaðý baþlat menüsü klasörünü seçiniz. Farklý bir isim girerek yeni bir klasör yaratabilirsiniz."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Kýsayollarý oluþturmadan devam et"
  
  !define MUI_TEXT_ABORTWARNING "$(^Name) kurulumundan çýkmak istediðinize emin misiniz?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "$(^Name) Programýný Kaldýrma Sihirbazýna Hoþ Geldiniz"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Bu sihirbaz size $(^Name) programýnýn kadýrýlýmý boyunca rehberlik edecektir.\r\n\r\nKaldýrým iþlemeni baþlatmadan önce çalýþan diðer programlari kapatmanýzý öneririz. Böylece bilgisayarýnýzý yeniden baþlatmadan bazý sistem dosyalarý sorunsuz kaldýrýlabilir.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "$(^Name) Programýný Kaldýr"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "$(^Name) programýný sisteminizden kaldýrma."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Lisans Sözleþmesi"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Lütfen $(^Name) programýný sisteminizden kaldýrmadan önce sözleþmeyi okuyunuz."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Sözleþme koþullarýný kabul ediyorsanýz, 'Kabul Ediyorum'a basýnýz. $(^Name) programýný sisteminizden kaldýrmak için sözleþme koþullarýný kabul etmelisiniz."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Sözleþme koþullarýný kabul ediyorsanýz, aþaðýdaki onay kutusunu doldurunuz. $(^Name) programýný sisteminizden kaldýrmak için sözleþme koþullarýný kabul etmelisiniz. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Sözleþme koþullarýný kabul ediyorsanýz, asagidaki onay düðmesini seçiniz. $(^Name) programýný sisteminizden kaldýrmak için sözleþme koþullarýný kabul etmelisiniz. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Bileþen Þeçimi"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Lütfen kaldýrmak istediðiniz $(^Name) program bileþenini seçiniz."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Kaldýrýlýcak Dizin Seçimi"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "$(^Name) programýný kaldýrmak istediginiz dizini seçiniz."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Kaldýrýlýyor"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Litfen $(^Name) programý sisteminizden kaldýrýlýrken bekleyiniz."
    
  !define MUI_UNTEXT_FINISH_TITLE "Kaldýrma Ýþlemi Tamamlandýr"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Kaldýrma iþlemi baþarýyla tamamlandý."
  
  !define MUI_UNTEXT_ABORT_TITLE "Kaldýrma Ýþlemi Ýptal Edildi"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Kaldýrma Ýþlemi tamamlanamadý."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "$(^Name) Programý Kaldýrma Sihirbazý Tamamlanýyor"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) programý sisteminizden kaldýrýldý.\r\n\r\nSihirbazý kapatmak için 'bitir'e basýnýz."
  
  !define MUI_UNTEXT_ABORTWARNING "$(^Name) Programi Kaldýrma iþleminden çýkmak istediðinize emin misiniz?"  
  
!insertmacro MUI_LANGUAGEFILE_END