;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: Greek (1032)
;By Makidis N. Mike

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "GREEK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Greek" ;Ελληνικά Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Κάντε κλικ στο Επόμενο για να συνεχίσετε."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Κάντε κλικ στο Εγκατάσταση για να αρχίσετε την εγκατάσταση."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Συμφωνία ’δειας Χρήσης"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Ελέγξτε τους όρους της άδειας χρήσης πριν εγκαταστήσετε το '${MUI_PRODUCT}'."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Πατήστε το Page Down για να δείτε το υπόλοιπο της άδειας χρήσης."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Εάν αποδέχεστε όλους τους όρους της συμφωνίας, κάντε κλικ στο Συμφωνώ για να συνεχίσετε. Πρέπει να αποδεχθείτε τη συμφωνία για να εγκαταστήσετε το '${MUI_PRODUCT}'."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Επιλογή Στοιχείων"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Επιλέξτε τα στοιχεία του '${MUI_PRODUCT}' που θέλετε να εγκαταστήσετε."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Επιλέξτε τα στοιχεία που θέλετε να εγκαταστήσετε και αποεπιλέξτε τα στοιχεία που δε θέλετε να εγκαταστήσετε."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Περιγραφή"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Περάστε το δείκτη του ποντικιού πάνω από ένα στοιχείο για να δείτε την περιγραφή του."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Επιλογή Θέσης Εγκατάστασης"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Επιλέξτε το φάκελο μέσα στον οποίο θα εγκατασταθεί το '${MUI_PRODUCT}'."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Το πρόγραμμα εγκατάστασης θα εγκαταστήσει το '${MUI_PRODUCT}' στον παρακάτω φάκελο.$\r$\n$\r$\nΓια να το εγκαταστήσετε σε έναν άλλο φάκελο, κάντε κλικ στο Αναζήτηση και επιλέξτε κάποιον άλλο φάκελο."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Φάκελος Εγκατάστασης"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Εγκατάσταση Σε Εξέλιξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Παρακαλώ περιμένετε όσο το '${MUI_PRODUCT}' εγκαθίσταται."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Ολοκληρώθηκε"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Η εγκατάσταση ολοκληρώθηκε επιτυχώς."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE "Φάκελος στο Μενού Έναρξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Επιλογή Φακέλου για το Μενού Έναρξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Επιλέξτε ένα φάκελο του μενού Έναρξη για τις συντομεύσεις του προγράμματος."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU "Επιλέξτε το φάκελο του μενού Έναρξη στον οποίο θέλετε να δημιουργήσετε τις συντομεύσεις του προγράμματος. Μπορείτε επίσης να εισάγετε ένα όνομα για να δημιουργήσετε ένα νέο φάκελο."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Είστε σίγουροι πως θέλετε να τερματίσετε την εγκατάσταση του '${MUI_PRODUCT}';"
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Απεγκατάσταση του '${MUI_PRODUCT}'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Αφαίρεση του '${MUI_PRODUCT}' από τον υπολογιστή σας."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Αυτό το πρόγραμμα θα απεγκαταστήσει το '${MUI_PRODUCT}' από τον υπολογιστή σας."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Απεγκατάσταση Σε Εξέλιξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Παρακαλώ περιμένετε όσο το '${MUI_PRODUCT}' απεγκαθίσταται."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Ολοκληρώθηκε"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Η απεγκατάσταση ολοκληρώθηκε επιτυχώς."
  
!insertmacro MUI_LANGUAGEFILE_END