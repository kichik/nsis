;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Mongolian (1104)
;By Bayarsaikhan Enkhtaivan

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "MONGOLIAN"

  !define MUI_LANGNAME "Mongolian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "$(^NameDA) Суулгацад тавтай морил"
  !define MUI_TEXT_WELCOME_INFO_TEXT "$(^NameDA) суулгацын илбэчинг та шууд ашиглаж болно.\r\n\r\nЇїнийг суулгахын ємнє бусад бїх програмуудаа хаахыг зєвлєж байна. Системийн файлуудыг шинэчилбэл компьютерээ дахин ачаалахгїй байх боломжтой.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Лицензийн зєвшєєрєл"  
  !define MUI_TEXT_LICENSE_SUBTITLE "$(^NameDA)-ыг суулгахынхаа ємнє зєвшилцлийн зїйлїїдийг уншина уу."
  !define MUI_INNERTEXT_LICENSE_TOP "Page Down товчийг даран зєвшилцлийг доош гїйлгэнэ її."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Хэрэв зєвшилцлийн зїйлсийг зєвшєєрч байвал, Зєвшєєрлєє товчийг даран їргэлжлїїлнэ її. $(^NameDA)-ыг суулгахын тулд заавал зєвшєєрєх шаардлагатай."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Хэрэв зєвшилцлийн зїйлсийг зєвшєєрч байвал, Зєвлєх хайрцгийг даран їргэлжлїїлнэ її. $(^NameDA)-ыг суулгахын тулд заавал зєвшєєрєх шаардлагатай. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Хэрэв зєвшилцлийн зїйлсийг зєвшєєрч байвал, доорхоос эхнийг нь сонгон їргэлжлїїлнэ її. $(^NameDA)-ыг суулгахын тулд заавал зєвшєєрєх шаардлагатай. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Нэгдлийг сонгох"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "$(^NameDA)-ыг суулгахад шаардагдах хэсгийг сонгоно уу."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Тайлбар"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Та хулганаараа нэгдлийн дээр очиход тїїний тайлбарыг харуулна."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Суулгах байрлалыг сонгох"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "$(^NameDA) суулгацын суулгах замыг сонго."
  
  !define MUI_TEXT_INSTALLING_TITLE "Суулгаж байна"
  !define MUI_TEXT_INSTALLING_SUBTITLE "$(^NameDA)-ыг суулгаж дуустал тїр хїлээнэ її."
  
  !define MUI_TEXT_FINISH_TITLE "Суулгаж дууслаа"
  !define MUI_TEXT_FINISH_SUBTITLE "Суулгац амжилттай болов."
  
  !define MUI_TEXT_ABORT_TITLE "Суулгалт таслагдлаа"
  !define MUI_TEXT_ABORT_SUBTITLE "Суулгалт амжилтгїй болов."
  
  !define MUI_BUTTONTEXT_FINISH "&Тєгсгєл"
  !define MUI_TEXT_FINISH_INFO_TITLE "$(^NameDA) Суулгацын илбэчин дууслаа"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) нь таны компьютерт суулаа.\r\n\r\nТєгсгєл дээр дарвал хаана."
  !define MUI_TEXT_FINISH_INFO_REBOOT "$(^NameDA)-ын суулгацын дараалалд та компьютерээ дахин ачаалснаар дуусна. Та дахин ачаалахыг хїсэж байна уу?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Д.Ачаал"
  !define MUI_TEXT_FINISH_REBOOTLATER "Би дараа д.ачаалахыг хїсэж байна."
  !define MUI_TEXT_FINISH_RUN "$(^NameDA) ажиллуулах"
  !define MUI_TEXT_FINISH_SHOWREADME "&Readme харуулах"
  
  !define MUI_TEXT_STARTMENU_TITLE "Start цэсний хавтсыг сонго"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Start цэс дэх $(^NameDA) shortcut-ын хавтсыг сонго."
  !define MUI_INNERTEXT_STARTMENU_TOP "Start цэсэнд програмын shortcut їїсгэх хавтсыг сонго. Эсвэл та шинэ нэрээр їїсгэж болно."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Do not create shortcuts"
  
  !define MUI_TEXT_ABORTWARNING "$(^Name) -ын суулгацаас гармаар байна уу?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "$(^NameDA) Суулгацыг устгах илбэчинд тавтай морил"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "$(^NameDA) устгацын илбэчинг та шууд ашиглаж болно.\r\n\r\nУстгахын ємнє $(^NameDA) нь ажиллаагїй эсэхийг шалга.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "$(^NameDA)--ын Устгац"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "$(^NameDA) -ыг таны компьютерээс зайлуулах."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Лицензийн зєвшєєрєл"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "$(^NameDA) устгахын ємнє зєвшилцлийн зїйлсийг уншина уу."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Хэрэв зєвшилцлийн зїйлсийг зєвшєєрч байвал, Зєвшєєрлєє товчийг даран їргэлжлїїлнэ її. $(^NameDA)-ыг устгахын тулд заавал зєвшєєрєх шаардлагатай."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Хэрэв зєвшилцлийн зїйлсийг зєвшєєрч байвал, Зєвлєх хайрцгийг даран їргэлжлїїлнэ її. $(^NameDA)-ыг устгахын тулд заавал зєвшєєрєх шаардлагатай. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Хэрэв зєвшилцлийн зїйлсийг зєвшєєрч байвал, доорхоос эхнийг нь сонгон їргэлжлїїлнэ її. $(^NameDA)-ыг устгахын тулд заавал зєвшєєрєх шаардлагатай. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Нэгдлийг сонгох"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "$(^NameDA)-ын устгах шаардлагатай нэгдлийг сонгох."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Устгацын байрлалыг сонгох"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "$(^NameDA)-ыг устгах хавтсыг сонгох."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Устгаж байна"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "$(^NameDA) -ыг зайлуулж дуустал тїр хїлээнэ її."
    
  !define MUI_UNTEXT_FINISH_TITLE "Устгаж дууслаа"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Устгалт амжилттай дууслаа."
  
  !define MUI_UNTEXT_ABORT_TITLE "Устгац таслагдлаа"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Устгалт амжилтгїй боллоо."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "$(^NameDA) Устгацын илбэчин дууслаа"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) нь таны компьютерээс зайлуулагдлаа.\r\n\r\nТєгсгєл дээр дарвал хаана."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "$(^NameDA) Устгацын дараалалд та компьютерээ дахин ачаалснаар дуусна. Та д.ачаалмаар байна уу?"
  
  !define MUI_UNTEXT_ABORTWARNING "$(^Name) Устгацаас гармаар байна уу?"
  
!insertmacro MUI_LANGUAGEFILE_END