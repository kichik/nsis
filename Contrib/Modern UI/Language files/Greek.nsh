;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.62

;Language: Greek (1032)
;By Makidis N. Mike

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "GREEK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Greek" ;(Ελληνικά) Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Κάντε κλικ στο Επόμενο για να συνεχίσετε."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Κάντε κλικ στο Εγκατάσταση για να αρχίσετε την εγκατάσταση."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Καλώς ήλθατε στην Εγκατάσταση του '${MUI_PRODUCT}'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ο οδηγός αυτός θα σας καθοδηγήσει κατά τη διάρκεια της εγκατάστασης του '${MUI_PRODUCT}'.\r\n\r\nΣυνιστάται να κλείσετε όλες τις άλλες εφαρμογές πριν ξεκινήσετε την Εγκατάσταση. Αυτό θα επιτρέψει στην Εγκατάσταση να ενημερώσει ορισμένα αρχεία συστήματος χωρίς την επανεκκίνηση του υπολογιστή σας.\r\n\r\n"
  
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
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Η Εγκατάσταση Ολοκληρώθηκε"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Η εγκατάσταση ολοκληρώθηκε επιτυχώς."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": Ολοκλήρωση"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Τέλος"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Ολοκλήρωση της Εγκατάστασης του '${MUI_PRODUCT}'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "Το '${MUI_PRODUCT}' εγκαταστάθηκε στον υπολογιστή σας.\r\n\r\nΚάντε κλικ στο Τέλος για να κλείσετε αυτόν τον οδηγό."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Πρέπει να γίνει επανεκκίνηση του υπολογιστή σας για να ολοκληρωθεί η εγκατάσταση του '${MUI_PRODUCT}'. Θέλετε να επανεκκινήσετε τον υπολογιστή σας τώρα;"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Να γίνει επανεκκίνηση τώρα"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Θα επανεκκινήσω τον υπολογιστή μου αργότερα"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Εκτέλεση του '${MUI_PRODUCT}'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Εμφάνιση του αρχείου Readme"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": Φάκελος στο Μενού Έναρξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Επιλογή Φακέλου για το Μενού Έναρξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Επιλέξτε ένα φάκελο του μενού Έναρξη για τις συντομεύσεις του προγράμματος."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Επιλέξτε ένα φάκελο του μενού Έναρξη για τις συντομεύσεις του προγράμματος. Μπορείτε επίσης να εισάγετε ένα όνομα για να δημιουργήσετε ένα νέο φάκελο."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Να μη δημιουργηθούν συντομεύσεις"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Είστε σίγουροι πως θέλετε να τερματίσετε την εγκατάσταση του '${MUI_PRODUCT}';"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Κάντε κλικ στο Απεγκατάσταση για να αρχίσετε την απεγκατάσταση."  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Απεγκατάσταση του '${MUI_PRODUCT}'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Αφαίρεση του '${MUI_PRODUCT}' από τον υπολογιστή σας."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Ο οδηγός αυτός θα απεγκαταστήσει το '${MUI_PRODUCT}' από τον υπολογιστή σας."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Απεγκατάσταση Σε Εξέλιξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Παρακαλώ περιμένετε όσο το '${MUI_PRODUCT}' απεγκαθίσταται."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Η Απεγκατάσταση Ολοκληρώθηκε"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Η απεγκατάσταση ολοκληρώθηκε επιτυχώς."
  
!insertmacro MUI_LANGUAGEFILE_END