;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Hebrew (1037)
;By Yaron Shahrabani

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "HEBREW"

  !define MUI_LANGNAME "Hebrew" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "ברוכים הבאים לאשף ההתקנה של $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "אשף זה ינחה אותכם במהלך ההתקנה של $(^Name).\r\n\r\nמומלץ לסגור כל תוכנית אחרת לפני התחלת ההתקנה. פעולה זו תאפשר לאשף לעדכן קבצי מערכת ללא איתחול המחשב.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "הסכם רישוי"
  !define MUI_TEXT_LICENSE_SUBTITLE ". $(^Name) אנא עיין בתנאי הרשיון לפני התקנת"
  !define MUI_INNERTEXT_LICENSE_TOP ".בכדי לצפות בשאר הרשיון Page Down לחץ על"
  !define MUI_INNERTEXT_LICENSE_BOTTOM ".אם לא תסכים לתנאי הרשיון$(^Name) אם אתה מקבל את תנאי הרשיון, לחץ על 'אני מסכים' כדי להמשיך. לא תוכל להתקין את"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "$_CLICK .$(^Name) אם אתה מקבל את תנאי ההסכם, סמן את תיבת הבחירה שלהלן. עלייך לקבל את תנאי ההסכם בכדי להתקין את"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "$_CLICK .$(^Name) אם אתה מקבל את תנאי ההסכם, בחר באפשרות הראשונה שלהלן. עלייך לקבל את ההסכם בכדי להתקין את"

  !define MUI_TEXT_COMPONENTS_TITLE "בחר רכיבים"
  !define MUI_TEXT_COMPONENTS_SUBTITLE ".ברצונך להתקין $(^Name) בחר אילו רכיבים של"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "תיאור"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO ".העבר את העכבר מעל רכיב כלשהו בכדי לצפות בתיאורו"

  !define MUI_TEXT_DIRECTORY_TITLE "בחר מיקום להתקנה"
  !define MUI_TEXT_DIRECTORY_SUBTITLE ".$(^Name) בחר את התיקייה בה אתה מעוניין להתקין את"

  !define MUI_TEXT_INSTALLING_TITLE "מתקין"
  !define MUI_TEXT_INSTALLING_SUBTITLE ".מותקן $(^Name) -אנא המתן בזמן ש "

  !define MUI_TEXT_FINISH_TITLE "ההתקנה הושלמה"
  !define MUI_TEXT_FINISH_SUBTITLE ".ההתקנה הושלמה במלואה"

  !define MUI_TEXT_ABORT_TITLE "ההתקנה בוטלה"
  !define MUI_TEXT_ABORT_SUBTITLE ".ההתקנה לא הושלמה המלואה"

  !define MUI_BUTTONTEXT_FINISH "&סיים"
  !define MUI_TEXT_FINISH_INFO_TITLE "$(^Name) משלים את אשף ההתקנה של"
  !define MUI_TEXT_FINISH_INFO_TEXT "הותקן בהצלחה $(^Name)\r\n\r\nלחץ על סיום בכדי .לסגור את האשף"
  !define MUI_TEXT_FINISH_INFO_REBOOT "?האם ברצונך לאתחל כעת .$(^Name) עלייך לאתחל את המחשב בכדי לסיים את התקנת"
  !define MUI_TEXT_FINISH_REBOOTNOW "אתחל כעת"
  !define MUI_TEXT_FINISH_REBOOTLATER "ברצוני לאתחל ידנית מאוחר יותר"
  !define MUI_TEXT_FINISH_RUN "$(^Name) &הרץ את"
  !define MUI_TEXT_FINISH_SHOWREADME "'&הצג מסמך 'קרא אותי"

  !define MUI_TEXT_STARTMENU_TITLE "בחר תיקייה בתפריט התחל"
  !define MUI_TEXT_STARTMENU_SUBTITLE ".בחר בתיקיית תפריט התחל בכדי לשים בה את קיצורי הדרך של התוכנית"
  !define MUI_INNERTEXT_STARTMENU_TOP ".בחר בתיקייה בתפריט התחל בה ברצונך למקם את קיצורי הדרך עבור התוכנית. באפשרותך גם להקיש את שם התיקייה בכדי ליצור תיקייה חדשה"
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "אל תיצור קיצורי דרך"

  !define MUI_TEXT_ABORTWARNING "?$(^Name) האם אתה בטוח שברצונך לצאת מהתקנת המוצר"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "ברוכים הבאים לאשף ההסרה של $(^Name)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "אשף זה ינחה אותכם במהלך ההסרה של $(^Name).\r\n\r\nמומלץ לסגור כל תוכנית אחרת לפני התחלת ההתקנה. פעולה זו תאפשר לאשף לעדכן קבצי מערכת ללא איתחול המחשב.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "$(^Name) הסר את"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE ".מהמחשב $(^Name) הסר את"

  !define MUI_UNTEXT_LICENSE_TITLE "הסכם רישוי"
  !define MUI_UNTEXT_LICENSE_SUBTITLE ". $(^Name) אנא עיין בתנאי הרשיון לפני הסרת"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM ".אם לא תסכים לתנאי הרשיון$(^Name) אם אתה מקבל את תנאי הרשיון, לחץ על 'אני מסכים' כדי להמשיך. לא תוכל להסיר את"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "$_CLICK .$(^Name) אם אתה מקבל את תנאי ההסכם, סמן את תיבת הבחירה שלהלן. עלייך לקבל את תנאי ההסכם בכדי להסיר את"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "$_CLICK .$(^Name) אם אתה מקבל את תנאי ההסכם, בחר באפשרות הראשונה שלהלן. עלייך לקבל את ההסכם בכדי להסיר את"

  !define MUI_UNTEXT_COMPONENTS_TITLE "בחר רכיבים"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE ".ברצונך להסיר $(^Name) בחר אילו תכונות של"

  !define MUI_UNTEXT_DIRECTORY_TITLE "בחר מיקום להסרה"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE ".$(^Name) בחר את התיקייה ממנה אתה מעוניין להסיר את"

  !define MUI_UNTEXT_UNINSTALLING_TITLE "מסיר"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE ".מוסר מהמחשב $(^Name) אנא המתן בזמן ש-"

  !define MUI_UNTEXT_FINISH_TITLE "ההסרה הושלמה"
  !define MUI_UNTEXT_FINISH_SUBTITLE ".ההסרה הושלמה במלואה"

  !define MUI_UNTEXT_ABORT_TITLE "ההסרה בוטלה"
  !define MUI_UNTEXT_ABORT_SUBTITLE ".ההסרה לא הושלמה במלואה"

  !define MUI_UNTEXT_FINISH_INFO_TITLE "$(^Name) משלים את אשף ההסרה של"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "הותקן בהצלחה $(^Name)\r\n\r\nלחץ על סיום בכדי .לסגור את האשף"
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "?האם ברצונך לאתחל כעת .$(^Name) עלייך לאתחל את המחשב בכדי לסיים את הסרת"

  !define MUI_UNTEXT_ABORTWARNING "?$(^Name) האם אתה בטוח שברצונך לצאת מהסרת"

!insertmacro MUI_LANGUAGEFILE_END
