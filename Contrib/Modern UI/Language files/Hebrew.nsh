;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Hebrew (1037)
;By Yaron Shahrabani

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "HEBREW"

  !define MUI_LANGNAME "Hebrew" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "ברוכים הבאים לאשף ההתקנה של $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "אשף זה ינחה אתכם במהלך ההתקנה של $(^NameDA).\r\n\r\nמומלץ לסגור כל תוכנית אחרת לפני התחלת ההתקנה. פעולה זו תאפשר לאשף לעדכן קבצי מערכת ללא איתחול המחשב.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "הסכם רישוי"
  !define MUI_TEXT_LICENSE_SUBTITLE "נא עיין בתנאי הסכם הרישוי לפני התקנת $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "כדי לצפות בשאר הסכם הרישוי לחץ על Page Down."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "אם אתה מקבל את תנאי ההסכם, לחץ על 'אני מסכים' כדי להמשיך. אם לא תסכים לתנאי ההסכם לא תוכל להתקין את $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "אם אתה מקבל את תנאי ההסכם, סמן את תיבת הבחירה שלהלן. עלייך לקבל את תנאי ההסכם בכדי להתקין את $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "אם אתה מקבל את תנאי ההסכם, בחר באפשרות הראשונה שלהלן. עלייך לקבל את ההסכם כדי להתקין את $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "בחר רכיבים"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "בחר אילו רכיבים של $(^NameDA) ברצונך להתקין."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "תיאור"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "העבר את העכבר מעל רכיב כלשהו בכדי לצפות בתיאורו."

  !define MUI_TEXT_DIRECTORY_TITLE "בחר מיקום להתקנה"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "בחר את התיקייה בה אתה מעוניין להתקין את $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "מתקין"
  !define MUI_TEXT_INSTALLING_SUBTITLE "אנא המתן בזמן ש-$(^NameDA) מותקן."

  !define MUI_TEXT_FINISH_TITLE "ההתקנה הושלמה"
  !define MUI_TEXT_FINISH_SUBTITLE "ההתקנה הושלמה במלואה."

  !define MUI_TEXT_ABORT_TITLE "ההתקנה בוטלה"
  !define MUI_TEXT_ABORT_SUBTITLE "ההתקנה לא הושלמה המלואה."

  !define MUI_BUTTONTEXT_FINISH "&סיים"
  !define MUI_TEXT_FINISH_INFO_TITLE "משלים את אשף ההתקנה של $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) הותקן בהצלחה.\r\n\r\nלחץ על סיום כדי לסגור את האשף."
  !define MUI_TEXT_FINISH_INFO_REBOOT "עלייך לאתחל את המחשב כדי לסיים את התקנת $(^NameDA). האם ברצונך לאתחל כעת?"
  !define MUI_TEXT_FINISH_REBOOTNOW "אתחל כעת"
  !define MUI_TEXT_FINISH_REBOOTLATER "ברצוני לאתחל ידנית מאוחר יותר"
  !define MUI_TEXT_FINISH_RUN "&הרץ את $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&הצג מסמך 'קרא אותי'"

  !define MUI_TEXT_STARTMENU_TITLE "בחר תיקייה בתפריט ההתחלה"
  !define MUI_TEXT_STARTMENU_SUBTITLE "בחר בתיקיית תפריט ההתחלה בה יווצרו קיצורי הדרך של התוכנית."
  !define MUI_INNERTEXT_STARTMENU_TOP "בחר בתיקייה מתפריט ההתחלה בה ברצונך ליצור את קיצורי הדרך עבור התוכנית. באפשרותך גם להקליד את שם התיקייה כדי ליצור תיקייה חדשה."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "אל תיצור קיצורי דרך"

  !define MUI_TEXT_ABORTWARNING "האם אתה בטוח שברצונך לצאת מהתקנת $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "ברוכים הבאים לאשף ההסרה של $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "אשף זה ינחה אתכם במהלך ההסרה של $(^NameDA).\r\n\r\nמומלץ לסגור כל תוכנית אחרת לפני התחלת ההסרה. פעולה זו תאפשר לאשף לעדכן קבצי מערכת ללא איתחול המחשב.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "הסר את $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "הסר את $(^NameDA) מהמחשב."

  !define MUI_UNTEXT_LICENSE_TITLE "הסכם רישוי"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "נא עיין בתנאי הסכם הרישוי לפני הסרת $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "אם אתה מקבל את תנאי ההסכם, לחץ על 'אני מסכים' כדי להמשיך. אם לא תקבל את תנאי ההסכם לא תוכל להסיר את $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "אם אתה מקבל את תנאי ההסכם, סמן את תיבת הבחירה שלהלן. עלייך לקבל את תנאי ההסכם כדי להסיר את $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "אם אתה מקבל את תנאי ההסכם, בחר באפשרות הראשונה שלהלן. עלייך לקבל את ההסכם כדי להסיר את $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "בחר רכיבים"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "בחר אילו תכונות של $(^NameDA) ברצונך להסיר."

  !define MUI_UNTEXT_DIRECTORY_TITLE "בחר מיקום להסרה"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "בחר את התיקייה ממנה אתה מעוניין להסיר את $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "מסיר"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "אנא המתן בזמן ש-$(^NameDA) מוסר מהמחשב."

  !define MUI_UNTEXT_FINISH_TITLE "ההסרה הושלמה"
  !define MUI_UNTEXT_FINISH_SUBTITLE "ההסרה הושלמה במלואה."

  !define MUI_UNTEXT_ABORT_TITLE "ההסרה בוטלה"
  !define MUI_UNTEXT_ABORT_SUBTITLE "ההסרה לא הושלמה במלואה."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "משלים את אשף ההסרה של $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) הוסר בהצלחה.\r\n\r\nלחץ על סיום כדי לסגור את האשף."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "עלייך לאתחל את המחשב כדי לסיים את הסרת $(^NameDA). האם ברצונך לאתחל כעת?"

  !define MUI_UNTEXT_ABORTWARNING "האם אתה בטוח שברצונך לצאת מהסרת $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END
