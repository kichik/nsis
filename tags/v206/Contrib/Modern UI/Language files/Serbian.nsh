;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Serbian (3098)
;Translation by Срђан Обућина <obucina@srpskijezik.edu.yu>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Serbian"

  !define MUI_LANGNAME "Serbian Cyrillic" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Добродошли у водич за инсталацију програма $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Бићете вођени кроз процес инсталације програма $(^NameDA).\r\n\r\nПрепоручљиво је да искључите све друге програме пре почетка инсталације. Ово може омогућити ажурирање системских фајлова без потребе за рестартовањем рачунара.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Договор о праву коришћења"
  !define MUI_TEXT_LICENSE_SUBTITLE "Пажљиво прочитајте договор о праву коришћења пре инсталације програма $(^NameDA)-e."
  !define MUI_INNERTEXT_LICENSE_TOP "Притисните Page Down да би видели остатак договора."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ако прихватате све услове договора, притисните дугме 'Прихватам' за наставак. Морате прихватити договор да би инсталирали програм $(^NameDA)-u."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако прихватате све услове договора, обележите квадратић испод. Морате прихватити договор да би инсталирали програм $(^NameDA)-u. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако прихватате све услове договора, изаберите прву опцију испод. Морате прихватити договор да би инсталирали програм $(^NameDA)-u. $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Избор компоненти за инсталацију"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Изаберите компоненте за инсталацију. Инсталирају се само означене компоненте."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Опис"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Пређите курсором миша преко имена компоненте да бисте видели њен опис."

  !define MUI_TEXT_DIRECTORY_TITLE "Избор фолдера за инсталацију"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Изаберите фолдер у који ћете инсталирати програм $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Инсталација"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Сачекајте док се програм $(^NameDA) инсталира."

  !define MUI_TEXT_FINISH_TITLE "Завршена инсталација"
  !define MUI_TEXT_FINISH_SUBTITLE "Инсталација је успешно завршена."

  !define MUI_TEXT_ABORT_TITLE "Прекинута инсталација"
  !define MUI_TEXT_ABORT_SUBTITLE "Инсталација је прекинута и није успешно завршена."

  !define MUI_BUTTONTEXT_FINISH "Крај"
  !define MUI_TEXT_FINISH_INFO_TITLE "Завршена инсталација програма $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Програм $(^NameDA) је инсталиран на рачунар.\r\n\r\nПритисните дугме 'Крај' за затварање овог прозора."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Рачунар мора бити рестартован да би се процес инсталације програма $(^NameDA) успешно завршио. Желите ли одмах да рестартујете рачунар?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Рестартуј одмах"
  !define MUI_TEXT_FINISH_REBOOTLATER "Без рестартовања"
  !define MUI_TEXT_FINISH_RUN "Покрени програм $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Прикажи ПрочитајМе фајл"

  !define MUI_TEXT_STARTMENU_TITLE "Избор фолдера у Старт менију"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Изаберите фолдер у Старт менију у коме ће се креирати пречице."
  !define MUI_INNERTEXT_STARTMENU_TOP "Изаберите фолдер у Старт менију у коме желите да буду креиране пречице програма. Можете уписати и име за креирање новог фолдера."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Без креирања пречица"

  !define MUI_TEXT_ABORTWARNING "Сигурно желите да прекинете инсталацију програма $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Добродошли у деинсталацију програма $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Бићете вођени кроз процес деинсталације програма $(^NameDA).\r\n\r\nПре почетка деинсталације, уверите се да је програм $(^NameDA) искључен. $_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Деинсталација програма $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Деинсталација програма $(^NameDA) са рачунара."

  !define MUI_UNTEXT_LICENSE_TITLE "Договор о праву коришћења"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Пажљиво прочитајте договор о праву коришћења пре деинсталације програма $(^NameDA)-e."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ако прихватате све услове договора, притисните дугме 'Прихватам' за наставак. Морате прихватити договор да би деинсталирали програм $(^NameDA)-u."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ако прихватате све услове договора, обележите квадратић испод. Морате прихватити договор да би деинсталирали програм $(^NameDA)-u. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ако прихватате све услове договора, изаберите прву опцију испод. Морате прихватити договор да би деинсталирали програм $(^NameDA)-u. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Избор компоненти за деинсталацију"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Изаберите компоненте за деинсталацију. Деинсталирају се само означене компоненте."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Избор фолдера за деинсталaцију"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Изаберите фолдер из кога ћете деинсталирати програм $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Деинсталација"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Сачекајте док се програм $(^NameDA) деинсталира."

  !define MUI_UNTEXT_FINISH_TITLE "Завршена деинсталација"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Деинсталација је успешно завршена."
  
  !define MUI_UNTEXT_ABORT_TITLE "Прекинута деинсталација"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Деинсталација је прекинута и није успешно завршена."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Завршена деинсталација програма $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Програм $(^NameDA) је деинсталиран са рачунара.\r\n\r\nПритисните дугме 'Крај' за затварање овог прозора."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Рачунар мора бити рестартован да би завршили деинсталацију програма $(^NameDA). Желите ли одмах да рестартујете рачунар?"

  !define MUI_UNTEXT_ABORTWARNING "Сигурно желите да прекинете деинсталацију програма $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END