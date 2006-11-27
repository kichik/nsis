;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Uzbek (1091)
;Translation updated by Emil Garipov [emil.garipov@gmail.com] 

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Uzbek"

  !define MUI_LANGNAME "Uzbek" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Sizni o'rnatish dastur tabriklaydi $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Bu dastur sizning komputeringizga $(^NameDA) dasturni o'rnatadi.\r\n\r\nO'rnatishdan oldin ishlayotgan barcha ilovalarni yopish tavsiya etiladi. Bu o'rnatuvchi dasturga kompyuterni qayta yuklamasdan sistemali fayllarni yangilash imkonini beradi.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Lisenzion kelishuv"  
  !define MUI_TEXT_LICENSE_SUBTITLE "$(^NameDA) dasturini o'rnatishdan oldin lisenzion kelishuv bilan tanishib chiking."
  !define MUI_INNERTEXT_LICENSE_TOP "Matn bo'icha silgish uchun $\"PageUp$\" va $\"PageDown$\" tugmasidan foydalaning."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Agar kelishuv shartlariga rozi bo'lsangiz $\"Qabul kilaman$\" tugmasini bosing.Dasturni o'rnatish uchun,kelishuv shartlarini qabul qilish kerak."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Agar siz kelishuv shartlarini qabul kilsangiz,bayroqchani joylashtiring. Dasturni o'rnatish uchun kelisuv shartlarini qabul qilish kerak. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Kelishuv shartlarini qabul qilsangiz quida taklif etilganlardan birinchi variantni tanlang. Dasturni o'rnatish uchun kelisuv shartlarini qabul qilish kerak. $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "O'rnatilayotgan dastur komponentlari"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "$(^NameDA) dasturning o'zingizga kerak bo'lgan komponentasini tanlang."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Tasvir"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Sichqonchaning kursorini komponent tasvirini o'qish uchun ustiga quying."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Tasvirini o'qish o'chun, komponentni tanlang."
  !endif

  !define MUI_TEXT_DIRECTORY_TITLE "O'rnatish papkasini tanlash"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "$(^NameDA)ni o'rnatish uchun papka tanlang."
 
  !define MUI_TEXT_INSTALLING_TITLE "Fayllarni ko'chirish"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Biror kuting, $(^NameDA) fayllari ko'chirilmoqda..."
   
  !define MUI_TEXT_FINISH_TITLE "O'rnatish jarayoni tugadi"
  !define MUI_TEXT_FINISH_SUBTITLE "O'rnatish jarayoni muvaffaqiyat bilan tugadi."
  
  !define MUI_TEXT_ABORT_TITLE "O'rnatish jarayoni uzildi"
  !define MUI_TEXT_ABORT_SUBTITLE "O'rnatish jarayoni tugamadi."
  
  !define MUI_BUTTONTEXT_FINISH "&Tayor"
  !define MUI_TEXT_FINISH_INFO_TITLE "$(^NameDA)ni o'rnatuvci dasturi o'z ishini tugatmoqda"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA)ni o'rnatish bajarildi.\r\n\r\nO'rnatuvchi dasturdan chiqish uchun $\"Tayor$\" tugmasini bosing."
  !define MUI_TEXT_FINISH_INFO_REBOOT "$(^NameDA) dasturini o'rnatish jarayonini tugatish uchun Kompyuterni qayta yuklash kerak.Shu ishni bajarishni xoziroq istaysizmi?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Ha, kompyuter hozir qayta yuklansin"
  !define MUI_TEXT_FINISH_REBOOTLATER "Yo'q, bu ishni keyinroq bajaraman"
  !define MUI_TEXT_FINISH_RUN "$(^NameDA) &Ishga tushirilsin"
  !define MUI_TEXT_FINISH_SHOWREADME "&Readme fayli ko'rsatilsin"
  
  !define MUI_TEXT_STARTMENU_TITLE "Papka $\"Пуск$\" menyusida"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Dastur belgilarini joylashtirish uchun $\"Пуск$\" menyusidan papka tanlang."
  !define MUI_INNERTEXT_STARTMENU_TOP "$\"Пуск$\" menyusidan dastur belgilari joylashadigan papka tanlang. Siz papkaning boshqa ismini kiritishingiz mumkin"
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Belgilar yaratilmasin"

  !define MUI_TEXT_ABORTWARNING "Haqiqatdan ham siz $(^Name)ni o'rnatishni bekor qilmoqchimisiz?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Sizni $(^NameDA)ni o'chirish dasturi tabriklaydi"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Bu dastur $(^NameDA)ni sizning kompyuteringizdan o'chiradi.\r\n\r\nO'chirishdan oldin $(^NameDA) dasturni ishlamayotganligini aniqlang.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "$(^NameDA)ni o'chirish"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "$(^NameDA)ni kompyuterdan o'chirish."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Lisenzion kelishuv"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "$(^NameDA)ni o'chirishdan oldin lesinzion kelishuv bilan tanishing."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Agar siz kelishuv shartlariniqabul qilsangiz $\"Qabul qilaman$\" tugmasini bosing. O'chirish uchun kelishuv shartlarini qabul qilishingiz kerak. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Agar shartlarni qabul qilsangiz, bayroqchani o'rnating.O'chirish uchun kelishuv shartlarini qabul qilishingiz kerak. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Kelishuv shartlarini qabul qilsangiz, taklif etilganlardan birinchi variantni tanlang.O'chirish uchun kelishuv shartlarini qabul qilishingiz kerak. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Dastur komponentlari"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "$(^NameDA)ning o'chirish kerak bo'lgan komponentlarini tanlang."
 
  !define MUI_UNTEXT_DIRECTORY_TITLE "O'chiriladigan papkani tanlash"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "$(^NameDA) o'chiriladigan papkasini ko'rsating."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "O'chirish"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Biror kutib turing, $(^NameDA) fayllarini o'chirish bajarilmoqda..."

  !define MUI_UNTEXT_FINISH_TITLE "O'chirish tuganlandi"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Dasturni o'chirish muvaffaqiyatli yakunlandi."

  !define MUI_UNTEXT_ABORT_TITLE "O'chirish jarayoni uzildi"
  !define MUI_UNTEXT_ABORT_SUBTITLE "O'chirish to'la bajarilmadi."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "$(^NameDA)ni o'chirish dasturi o'z ishini tugatdi."
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) dasturi kompyuteringizdan o'chirildi.\r\n\r\nO'chirish dasturidan chiqish uchun $\"Tayor$\"tugmasini bosing."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "$(^NameDA) dasturini o'chirishni tugatish uchun kompyuterni qayta yuklash kerak.shu ishni xozir bajarasizmi?"

  !define MUI_UNTEXT_ABORTWARNING "$(^Name)ni o'chirish jarayonini bekor qilmoqchisizmi?"

!insertmacro MUI_LANGUAGEFILE_END