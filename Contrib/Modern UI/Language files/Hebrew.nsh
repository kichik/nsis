;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Hebrew (1037)
;By Yaron Shahrabani

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "HEBREW"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Hebrew - עברית" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT ".לחץ על 'הבא' כדי להמשיך"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL ".לחץ על 'התקן' בכדי להתחיל בהתקנה"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "${MUI_PRODUCT} ברוכים הבאים לאשף ההתקנה של"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "\r\n\r\n.מומלץ לסגור כל תוכנית אחרת לפני התחלת ההתקנה. פעולה זו תאפשר לאשף לעדכן קבצי מערכת ללא איתחול המחשב\r\n\r\n .${MUI_PRODUCT} אשף זה ינחה אותכם במהלך ההתקנה של"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "הסכם רישוי"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE ". ${MUI_PRODUCT} אנא עיין בתנאי הרשיון לפני התקנת"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP ".בכדי לצפות בשאר הרשיון Page Down לחץ על"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM ".אם לא תסכים לתנאי הרשיון${MUI_PRODUCT} אם אתה מקבל את תנאי הרשיון, לחץ על 'אני מסכים' כדי להמשיך. לא תוכל להתקין את"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX ".${MUI_PRODUCT} אם אתה מקבל את תנאי ההסכם, סמן את תיבת הבחירה שלהלן. עלייך לקבל את תנאי ההסכם בכדי להתקין את"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS ".${MUI_PRODUCT} אם אתה מקבל את תנאי ההסכם, בחר באפשרות הראשונה שלהלן. עלייך לקבל את ההסכם בכדי להתקין את"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "בחר רכיבים"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE ".ברצונך להתקין ${MUI_PRODUCT} בחר אילו תכונות של"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP ".בחר את הרכיבים שברצונך להתקין ובטל את הסימון ליד פריטים שאינך מעוניין להתקין"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "תיאור"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO ".העבר את העכבר מעל רכיב כלשהו בכדי לצפות בתיאורו"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "בחר מיקום להתקנה"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE ".${MUI_PRODUCT} בחר את התיקייה בה אתה מעוניין להתקין את"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP ".בתיקיה ${MUI_PRODUCT} האשף יתקין את $\r$\n$\r$\nבכדי .להתקין לתיקייה אחרת, לחץ על סייר ובחר בתיקיה אחרת"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "תיקיית יעד"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "מתקין"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE ".מותקן ${MUI_PRODUCT} -אנא המתן בזמן ש "
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "ההתקנה הושלמה"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE ".ההתקנה הושלמה במלואה"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "ההתקנה בוטלה"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE ".ההתקנה לא הושלמה המלואה"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&סיים"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "${MUI_PRODUCT} משלים את אשף ההתקנה של"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "הותקן בהצלחה ${MUI_PRODUCT}\r\n\r\nלחץ על סיום בכדי .לסגור את האשף"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "?האם ברצונך לאתחל כעת .${MUI_PRODUCT} עלייך לאתחל את המחשב בכדי לסיים את התקנת"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "אתחל כעת"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "ברצוני לאתחל ידנית מאוחר יותר"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "${MUI_PRODUCT} &הרץ את"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "'&הצג מסמך 'קרא אותי"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "בחר תיקייה בתפריט התחל"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE ".בחר בתיקיית תפריט התחל בכדי לשים בה את קיצורי הדרך של התוכנית"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP ".בחר בתיקייה בתפריט התחל בה ברצונך למקם את קיצורי הדרך עבור התוכנית. באפשרותך גם להקיש את שם התיקייה בכדי ליצור תיקייה חדשה"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "אל תיצור קיצורי דרך"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "?${MUI_PRODUCT} האם אתה בטוח שברצונך לצאת מהתקנת המוצר"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL ".לחץ על 'הסר' כדי להתחיל בתוכנית ההסרה"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "${MUI_PRODUCT} הסר את"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE ".מהמחשב ${MUI_PRODUCT} הסר את"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO ".מהמחשב ${MUI_PRODUCT} אשף זה יסיר את"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "מסיר"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE ".מוסר מהמחשב ${MUI_PRODUCT} אנא המתן בזמן ש-"
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "ההסרה הושלמה"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE ".ההסרה הושלמה במלואה"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "ההסרה בוטלה"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE ".ההסרה לא הושלמה במלואה"
  
!insertmacro MUI_LANGUAGEFILE_END
