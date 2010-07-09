;Language: Farsi (1065)
;By FzerorubigD - FzerorubigD@gmail.com - Thanx to all people help me in forum.persiantools.com

!insertmacro LANGFILE "Farsi" "Farsi" "فارسی"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "به برنامه نصب  $(^NameDA) خوش آمديد."
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TEXT "اين برنامه شما را در نصب  $(^NameDA) ياري ميكند.$\r$\n$\r$\nتوصيه ميكنيم كليه برنامه هاي در حال اجرا را ببنديد. اين به برنامه نصب اجازه ميدهد كه فايلهاي لازم را بدون نياز به راه اندازي دوباره كامپيوتر شما به روز كند.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TITLE "به برنامه حذف $(^NameDA) خوش آمديد."
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TEXT " اين برنامه براي حذف $(^NameDA) به شما كمك ميكند.$\r$\n$\r$\nقبل از حذف  $(^NameDA) مطمئن شويد اين برنامه در حال اجرا نباشد.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "توافقنامه نصب"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "لطفا پيش از نصب $(^NameDA) مفاد توافقنامه را مرور كنيد."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "اگر كليه بندهاي توافقنامه را قبول داريد دكمه موافقم را بفشاريد. براي نصب  $(^NameDA) شما بايست اين توافقنامه را قبول كنيد."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "اگر كليه بندهاي توافقنامه را قبول داريد تيك زير را انتخاب كنيد. براي نصب  $(^NameDA) شما بايست اين توافقنامه را قبول كنيد. $_CLICK"
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "اگر كليه بندهاي توافقنامه را قبول داريد گزينه اول را انتخاب كنيد. براي نصب  $(^NameDA) شما بايست اين توافقنامه را قبول كنيد. $_CLICK"
!endif

!ifdef MUI_UNLICENSEPAGE
  ${LangFileString} MUI_UNTEXT_LICENSE_TITLE "توافقنامه حذف"
  ${LangFileString} MUI_UNTEXT_LICENSE_SUBTITLE "لطفا كليه بندهاي اين توافقنامه را قبل ار حذف $(^NameDA) مرور كنيد."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM "اگر كليه بندهاي توافقنامه را قبول داريد دكمه موافقم را بفشاريد. براي حذف $(^NameDA) شما بايست اين توافقنامه را قبول كنيد."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "اگر كليه بندهاي توافقنامه را قبول داريد تيك زير را انتخاب كنيد. براي حذف $(^NameDA) شما بايست اين توافقنامه ر قبول كنيد. $_CLICK"
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "اگر كليه بندهاي توافقنامه را قبول داريد گزينه اول را انتخاب كنيد. براي حذف $(^NameDA) شما بايست اين توافقنامه ر قبول كنيد. $_CLICK"
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "براي ديدن متن به صورت كامل از كليد Page Down استفاده كنيد."
!endif

!ifdef MUI_COMPONENTSPAGE
  ${LangFileString} MUI_TEXT_COMPONENTS_TITLE "انتخاب اجزاي برنامه "
  ${LangFileString} MUI_TEXT_COMPONENTS_SUBTITLE "اجزايي از  $(^NameDA) كه ميخواهيد نصب شوند را انتخاب كنيد."
!endif

!ifdef MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_UNTEXT_COMPONENTS_TITLE "انتخاب اجزاي برنامه"
  ${LangFileString} MUI_UNTEXT_COMPONENTS_SUBTITLE "اجزايي از $(^NameDA) را كه ميخواهيد حذف كنيد انتخاب كنيد."
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "توضيحات"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "نشانگر ماوس را بر روي  اجزايي كه ميخواهيد ببريد تا توضيحات آن را ببينيد."
  !else
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "نشانگر ماوس را بر روي  اجزايي كه ميخواهيد ببريد تا توضيحات آن را ببينيد."
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  ${LangFileString} MUI_TEXT_DIRECTORY_TITLE "انتخاب پوشه نصب"
  ${LangFileString} MUI_TEXT_DIRECTORY_SUBTITLE "پوشه اي كه ميخواهيد  $(^NameDA) در آن نصب شود را انتخاب كنيد."
!endif

!ifdef MUI_UNDIRECTORYPAGE
  ${LangFileString} MUI_UNTEXT_DIRECTORY_TITLE "پوشه حذف را انتخاب كنيد"
  ${LangFileString} MUI_UNTEXT_DIRECTORY_SUBTITLE "پوشه اي كه ميخواهيد $(^NameDA) را از آن حذف كنيد انتخاب كنيد."
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "نصب برنامه"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "لطفا مدت زماني كه  $(^NameDA) در حال نصب است را صبر كنيد."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "نصب پايان يافت"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "برنامه نصب با موفقيت پايان يافت."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "برنامه نصب لغو شد."
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "برنامه نصب به صورت نيمه تمام پايان يافت."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "حذف برنامه"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "لطفا مدت زماني كه  $(^NameDA) در حال حذف است را صبر كنيد."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "حذف پايان يافت"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "برنامه حذف با موفقيت پايان يافت."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "برنامه حذف لغو شد"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "برنامه حذف به صورت نيمه تمام پايان يافت"
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "برنامه نصب  $(^NameDA) پايان يافت"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) بر روي كامپيوتر شما نصب شد.$\r$\n$\r$\nبر روي دكمه پايان براي خروج از اين برنامه كليك كنيد."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "كامپيوتر شما براي تكميل نصب $(^NameDA) بايستي دوباره راه اندازي شود. آيا ميخواهيد اين كار را الان انجام دهيد؟"
!endif

!ifdef MUI_UNFINISHPAGE
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TITLE "برنامه حذف $(^NameDA) پايان يافت"
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) از روي كامپيوتر شما حذف شد.$\r$\n$\r$\nبر روي دكمه پايان براي خروج از اين برنامه كليك كنيد."
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_REBOOT "كامپيوتر شما براي تكميل حذف$(^NameDA) بايست دوباره راه اندازي شود.آيا ميخواهيد اين كار را الان انجام دهيد؟"
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "راه اندازي مجدد."
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "من خودم اين كار را انجام خواهم داد."
  ${LangFileString} MUI_TEXT_FINISH_RUN "&اجراي  $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&نمايش فايل توضيحات"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&پايان"  
!endif

!ifdef MUI_STARTMENUPAGE
  ${LangFileString} MUI_TEXT_STARTMENU_TITLE "انتخاب پوشه در منوي برنامه ها"
  ${LangFileString} MUI_TEXT_STARTMENU_SUBTITLE "پوشه اي كه ميخواهيد ميانبرهاي  $(^NameDA) در آن قرار بگيرند را انتخاب كنيد."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_TOP "پوشه اي در منوي برنامه ها كه ميخواهيد ميانبرهاي برنامه در آنجا ايجاد شوند را انتخاب كنيد. براي ايجاد يك پوشه جديد ميتوانيد يك نام تايپ كنيد."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_CHECKBOX "ميانبري نساز"
!endif

!ifdef MUI_UNCONFIRMPAGE
  ${LangFileString} MUI_UNTEXT_CONFIRM_TITLE "حذف $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_CONFIRM_SUBTITLE "پاك كردن $(^NameDA) از روي كامپيوتر شما."
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "آيا مطمئنيد كه ميخواهيد از برنامه نصب $(^Name) خارج شويد؟"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "آيا مطمئنيد كه ميخواهيد از برنامه حذف  $(^Name) خارج شويد؟"
!endif
