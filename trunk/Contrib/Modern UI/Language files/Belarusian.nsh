;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Belarusian (1059)
;Translated by Sitnikov Vjacheslav [ glory_man@tut.by ]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Belarusian"

  !define MUI_LANGNAME "Byelorussian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Вас вiтае майстар ўстаноўкі $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Гэтая праграма ўсталюе $(^NameDA) на Ваш кампутар.\r\n\r\nПерад пачаткам устаноўкi прапануем зачыніць усе праграмы, якія выконваюцца ў сапраўдны момант. Гэта дапаможа праграме ўстаноўкі абнавіць сістэмныя файлы без перазагрузкі кампутара.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Лiцензiйнае пагадненне"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Калі ласка, прачытайце ўмовы Ліцэнзійнага пагаднення перад пачаткам устаноўкi $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Выкарыстоўвайце кнопкi $\"PageUp$\" i $\"PageDown$\" для перамяшчэння па тэксце."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Калi Вы прымаеце ўмовы Лiцензiйнага пагаднення, націсніце кнопку $\"Згодзен$\". Гэта неабходна для ўстаноўкі праграмы."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Калi Вы прымаеце ўмовы Лiцензiйнага пагаднення, усталюйце сцяжок ніжэй. Гэта неабходна для ўстаноўкі праграмы. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Калi Вы прымаеце ўмовы Ліцэнзійнага пагаднення, вылучыце першы варыянт з прапанованых ніжэй. Гэта неабходна для ўстаноўкі праграмы. $_CLICK"  

  !define MUI_TEXT_COMPONENTS_TITLE "Кампаненты праграмы, якая ўсталёўваецца"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Вызначце кампаненты $(^NameDA), якія Вы жадаеце ўсталяваць."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Апісанне"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Усталюйце курсор мышы на назву кампанента, каб прачытаць яго апісанне."
   !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Вызначце кампанент, каб прачытаць яго апісанне."
   !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Выбар папкі ўстаноўкі"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Вызначце папку для ўстаноўкі $(^NameDA)."
 
  !define MUI_TEXT_INSTALLING_TITLE "Капіраванне файлаў"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Пачакайце, калі ласка, выконваецца капіраванне файлаў $(^NameDA) на Ваш кампутар..."
   
  !define MUI_TEXT_FINISH_TITLE "Устаноўка завершана"
  !define MUI_TEXT_FINISH_SUBTITLE "Устаноўка паспяхова завершана."
  
  !define MUI_TEXT_ABORT_TITLE "Устаноўка перарвана"
  !define MUI_TEXT_ABORT_SUBTITLE "Устаноўка не завершана."
  
  !define MUI_BUTTONTEXT_FINISH "&Гатова"
  !define MUI_TEXT_FINISH_INFO_TITLE "Заканчэнне майстра ўстаноўкі $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Устаноўка $(^NameDA) выканана.\r\n\r\nНацісніце кнопку $\"Гатова$\" для выйсця з праграмы ўстаноўкі."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Каб закончыць устаноўку $(^NameDA), неабходна перазагрузіць кампутар. Ці жадаеце Вы зрабіць гэта зараз?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Так, перазагрузіць кампутар зараз"
  !define MUI_TEXT_FINISH_REBOOTLATER "Не, перазагрузіць кампутар пазней"
  !define MUI_TEXT_FINISH_RUN "&Запусціць $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Паказаць інфармацыю аб праграме"
  
  !define MUI_TEXT_STARTMENU_TITLE "Папка ў меню $\"Пуск$\""
  !define MUI_TEXT_STARTMENU_SUBTITLE "Вылучыце папку ў меню $\"Пуск$\" для размяшчэння ярлыкоў праграмы."
  !define MUI_INNERTEXT_STARTMENU_TOP "Вылучыце папку ў меню $\"Пуск$\", куды будуць змешчаны ярлыкі праграмы. Вы таксама можаце азначыць іншае імя папкі."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Не ствараць ярлыкі"

  !define MUI_TEXT_ABORTWARNING "Вы сапраўды жадаеце скасаваць устаноўку $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Вас вiтае майстар выдалення $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Гэтая праграма выдаліць $(^NameDA) з Вашага кампутара.\r\n\r\nПерад пачаткам выдалення пераканайцеся ў тым, што праграма $(^NameDA) не выконваецца.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Выдаленне $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Выдаленне $(^NameDA) з Вашага кампутара."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Ліцэнзійнае пагадненне"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Калі ласка, прачытайце ўмовы Ліцэнзійнага пагаднення перад пачаткам выдалення $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Калі Вы прымаеце ўмовы Ліцэнзійнага пагаднення, націсніце кнопку $\"Згодзен$\". Гэта неабходна для выдалення праграмы. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Калі Вы прымаеце ўмовы Ліцэнзійнага пагаднення, усталюйце сцяжок ніжэй. Гэта неабходна для выдалення праграмы. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Калі Вы прымаеце ўмовы Ліцэнзійнага пагаднення, вылучыце першы варыянт з прапанаваных ніжэй. Гэта неабходна для выдалення праграмы. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Кампаненты праграмы"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Вызначце кампаненты $(^NameDA), якія Вы жадаеце выдаліць."
 
  !define MUI_UNTEXT_DIRECTORY_TITLE "Выбар папкі для выдалення"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Вызначце папку, з якой патрэбна выдаліць $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Выдаленне"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Пачакайце, калі ласка, выконваецца выдаленне файлаў $(^NameDA) з Вашага кампутара..."

  !define MUI_UNTEXT_FINISH_TITLE "Выдаленне завершана"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Выдаленне праграмы паспяхова завершана."

  !define MUI_UNTEXT_ABORT_TITLE "Выдаленне перарвана"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Выдаленне выканана не поўнасцю."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Заканчэнне работы майстара выдалення $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Праграма $(^NameDA) выдалена з Вашага кампутара.\r\n\r\nНацісніце кнопку $\"Гатова$\"каб выйсці з праграмы выдалення."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Каб скончыць выдаленне  $(^NameDA), неабходна перазагрузіць кампутар. Ці жадаеце Вы зрабіць гэта зараз?"

  !define MUI_UNTEXT_ABORTWARNING "Вы сапраўды жадаеце скасаваць выдаленне $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END