;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Greek (1032)
;By Makidis N. Michael - http://dias.aueb.gr/~p3010094/

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "GREEK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Greek" ;(Ελληνικά) Name of the language in the language itself
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Καλώς ήλθατε στην Εγκατάσταση του '$(^Name)'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ο οδηγός αυτός θα σας καθοδηγήσει κατά τη διάρκεια της εγκατάστασης του '$(^Name)'.\r\n\r\nΣυνιστάται να κλείσετε όλες τις άλλες εφαρμογές πριν ξεκινήσετε την Εγκατάσταση. Αυτό θα επιτρέψει στην Εγκατάσταση να ενημερώσει ορισμένα αρχεία συστήματος χωρίς την επανεκκίνηση του υπολογιστή σας.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Συμφωνία ’δειας Χρήσης"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Ελέγξτε την άδεια χρήσης πριν εγκαταστήσετε το '$(^Name)'."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Πατήστε το Page Down για να δείτε το υπόλοιπο της άδειας χρήσης."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Εάν αποδέχεστε τους όρους της άδειας χρήσης, κάντε κλικ στο Συμφωνώ για να συνεχίσετε. Πρέπει να αποδεχθείτε τη συμφωνία για να εγκαταστήσετε το '$(^Name)'."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Εάν αποδέχεστε τους όρους της άδειας χρήσης, κάντε κλικ στην επιλογή παρακάτω. Πρέπει να αποδεχθείτε τη συμφωνία για να εγκαταστήσετε το '$(^Name)'. $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Εάν αποδέχεστε τους όρους της άδειας χρήσης, κάντε κλικ στην πρώτη επιλογή παρακάτω. Πρέπει να αποδεχθείτε τη συμφωνία για να εγκαταστήσετε το '$(^Name)'. $_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Επιλογή Στοιχείων"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Επιλέξτε τα στοιχεία του '$(^Name)' που θέλετε να εγκαταστήσετε."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Περιγραφή"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Περάστε το δείκτη του ποντικιού πάνω από ένα στοιχείο για να δείτε την περιγραφή του."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Επιλογή Θέσης Εγκατάστασης"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Επιλέξτε το φάκελο μέσα στον οποίο θα εγκατασταθεί το '$(^Name)'."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Εγκατάσταση Σε Εξέλιξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Παρακαλώ περιμένετε όσο το '$(^Name)' εγκαθίσταται."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Η Εγκατάσταση Ολοκληρώθηκε"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Η εγκατάσταση ολοκληρώθηκε επιτυχώς."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Η Εγκατάσταση Διακόπηκε"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Η εγκατάσταση δεν ολοκληρώθηκε επιτυχώς."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Τέλος"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Ολοκλήρωση της Εγκατάστασης του '$(^Name)'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "Το '$(^Name)' εγκαταστάθηκε στον υπολογιστή σας.\r\n\r\nΚάντε κλικ στο Τέλος για να κλείσετε αυτόν τον οδηγό."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Πρέπει να γίνει επανεκκίνηση του υπολογιστή σας για να ολοκληρωθεί η εγκατάσταση του '$(^Name)'. Θέλετε να επανεκκινήσετε τον υπολογιστή σας τώρα;"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Να γίνει επανεκκίνηση τώρα"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Θα επανεκκινήσω τον υπολογιστή μου αργότερα"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Εκτέλεση του '$(^Name)'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Εμφάνιση του &αρχείου Readme"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Επιλογή Φακέλου για το Μενού Έναρξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Επιλέξτε ένα φάκελο του μενού Έναρξη για τις συντομεύσεις του '$(^Name)'."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Επιλέξτε ένα φάκελο του μενού Έναρξη για τις συντομεύσεις του προγράμματος. Μπορείτε επίσης να εισάγετε ένα όνομα για να δημιουργήσετε ένα νέο φάκελο."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Να μη δημιουργηθούν συντομεύσεις"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Είστε σίγουροι πως θέλετε να τερματίσετε την εγκατάσταση του '$(^Name)';"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Κάντε κλικ στο Απεγκατάσταση για να αρχίσετε την απεγκατάσταση."  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Απεγκατάσταση του '$(^Name)'"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Αφαίρεση του '$(^Name)' από τον υπολογιστή σας."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_TITLE "Συμφωνία ’δειας Χρήσης"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_SUBTITLE "Ελέγξτε την άδεια χρήσης πριν απεγκαταστήσετε το '$(^Name)'."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM "Εάν αποδέχεστε τους όρους της άδειας χρήσης, κάντε κλικ στο Συμφωνώ για να συνεχίσετε. Πρέπει να αποδεχθείτε τη συμφωνία για να απεγκαταστήσετε το '$(^Name)'."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Εάν αποδέχεστε τους όρους της άδειας χρήσης, κάντε κλικ στην επιλογή παρακάτω. Πρέπει να αποδεχθείτε τη συμφωνία για να απεγκαταστήσετε το '$(^Name)'. $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Εάν αποδέχεστε τους όρους της άδειας χρήσης, κάντε κλικ στην πρώτη επιλογή παρακάτω. Πρέπει να αποδεχθείτε τη συμφωνία για να απεγκαταστήσετε το '$(^Name)'. $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Επιλογή Στοιχείων"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Επιλέξτε τα στοιχεία του '$(^Name)' που θέλετε να απεγκαταστήσετε."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_TITLE "Επιλογή Θέσης Απεγκατάστασης"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_SUBTITLE "Επιλέξτε το φάκελο από τον οποίο θα απεγκατασταθεί το '$(^Name)'."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Απεγκατάσταση Σε Εξέλιξη"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Παρακαλώ περιμένετε όσο το '$(^Name)' απεγκαθίσταται."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Η Απεγκατάσταση Ολοκληρώθηκε"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Η απεγκατάσταση ολοκληρώθηκε επιτυχώς."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Η Απεγκατάσταση Διακόπηκε"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Η απεγκατάσταση δεν ολοκληρώθηκε επιτυχώς."
  
!insertmacro MUI_LANGUAGEFILE_END