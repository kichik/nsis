;NSIS Modern User Interface - Turkish Language File
;Compatible with Modern UI 1.63

;Language: Turkish (1055)
;By Bertan Kodamanoglu (astoichen@soultrap.com)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "TURKISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Turkish";
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "'Devam'a týklayýn."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Kurulumu baþlatmak için KUR'a týklayýnýz"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "${MUI_PRODUCT} Kurulumuna hoþgeldiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "\r\nKurulumu baþlatmadan önce çalýþan diðer programlarý kapatmanýzý öneririz. Böylece bilgisayarýnýzý yeniden baþlatmadan bazý sistem dosyalarý sorunsuz kurulabilir.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Sözleþme"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Lütfen programý kurmadan önce sözleþmeyi okuyunuz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Page Down'a basarak sözleþmenin tamamýný görebilirsiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Sözleþmeyi kabul ediyorsanýz 'Kabul'e basýn. Kurulumu baþlatabilmek için sözleþmeyi kabul etmeniz gerekli."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Paket Seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Lütfen ${MUI_PRODUCT} için kurmak istediðiniz paketleri seçin."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Paket seçimini tamamladýktan sonra"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Açýklama"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Paketlerin açýklamalarýný görmek için mouse ile üzerine gelin."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Hedef dizini seçin"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Programý kurmak istediðiniz dizini seçin"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} aþaðýda belirttiðiniz dizine kurulacak.$\r$\n$\r$\nFarklý bir dizine kurmak istiyorsanýz, 'Farklý Dizin'e týklayýn."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Hedef Dizin"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Kuruluyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Lütfen ${MUI_PRODUCT} kurulurken bekleyiniz."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Kurulum Tamamlandý!"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "${MUI_PRODUCT} kurulumu sorunsuz bir þekilde sonlandý."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Bitir"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "${MUI_PRODUCT} Kurulumu Tamamlanýyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} baþarýyla bilgisayarýnýza yüklendi.\r\n\r\nLütfen 'Bitir'e basarak kurulumu sonlandýrýn."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "${MUI_PRODUCT} kurulumunun tamamlanmasý için bilgisayarýnýzý yeniden baþlatmanýz gerekli. Bunu þimdi yapmak istiyor musunuz?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Evet"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Hayýr"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Programý çalýþtýr"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Yardým dosyasýný göster"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Baþlat menüsü klasörü seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Programýn kýsayollarýnýn konulacaðý baþlat menüsü klasörünü seçimi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Programýn kýsayollarýnýn konulacaðý baþlat menüsü klasörü seçin. Ayný zamanda farklý bir isim girerek yeni bir klasör yaratabilirsiniz."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Kýsayol yaratýmý"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "${MUI_PRODUCT} kurulumundan çýkmak istediðinizden emin misiniz?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Programý kaldýrmak istiyorsanýz 'Kaldýr' a basýn."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "${MUI_PRODUCT} Kaldýrýlýyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "${MUI_PRODUCT} bilgisayarýnýzdan silinmek üzere"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO ""
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Kaldýrýlýyor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Lütfen ${MUI_PRODUCT} kaldýrýlýrken bekleyiniz."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Tamamlandý"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Kaldýrma iþlemi baþarýyla tamamlandý."
  
!insertmacro MUI_LANGUAGEFILE_END