;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: Greek (1032)
;By Makidis N. Mike

;--------------------------------

!ifndef MUI_GREEK_USED

!define MUI_GREEK_USED

  !define MUI_GREEK_LANGNAME "Ελληνικά" ;Name of the language in the language itself (English, Deutsch, Franηais etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
    LicenseText /LANG=${LANG_GREEK} "Πατήστε το Page Down για να δείτε το υπόλοιπο της άδειας χρήσης."
    LangString MUI_TEXT_LICENSE_TITLE ${LANG_GREEK} "Συμφωνία ’δειας Χρήσης"  
    LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_GREEK} "Ελέγξτε τους όρους της άδειας χρήσης πριν εγκαταστήσετε το '${NAME}'."
    LangString MUI_INNERTEXT_LICENSE ${LANG_GREEK} "Για να συνεχιστεί η εγκατάσταση πρέπει να αποδεχθείτε όλους τους όρους της άδειας χρήσης. Αν τους αποδέχεστε, κάντε κλικ στο Συμφωνώ, αλλιώς κάντε κλικ στο ’κυρο για να τερματιστεί η εγκατάσταση του '${NAME}'."
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_GREEK} "Επιλέξτε τα μέρη που θέλετε να εγκαταστήσετε και αποεπιλέξτε αυτά που δε θέλετε να εγκαταστήσετε. Κάντε κλικ στο Επόμενο για να συνεχίσετε."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_GREEK} "Επιλογή Στοιχείων"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_GREEK} "Επιλέξτε τα στοιχεία που θέλετε να εγκαταστήσετε."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_GREEK} "Περιγραφή"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_GREEK} "Περάστε το δείκτη του ποντικιού πάνω από ένα στοιχείο για να δείτε την περιγραφή του."
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_GREEK} "Το πρόγραμμα εγκατάστασης θα εγκαταστήσει το '${NAME}' στον παρακάτω φάκελο.$\r$\n$\r$\nΓια να το εγκαταστήσετε στο φάκελο αυτό, κάντε κλικ στο Εγκατάσταση. Για να το εγκαταστήσετε σε έναν άλλο φάκελο, κάντε κλικ στο Αναζήτηση και επιλέξτε κάποιον άλλο φάκελο." " "
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_GREEK} "Επιλογή Θέσης Εγκατάστασης"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_GREEK} "Επιλέξτε το φάκελο μέσα στον οποίο θα εγκατασταθεί το '${NAME}'."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_GREEK} "Φάκελος εγκατάστασης"
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_GREEK} "Επόμενο >"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_GREEK} "Εγκατάσταση Σε Εξέλιξη"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_GREEK} "Παρακαλώ περιμένετε όσο το '${NAME}' εγκαθίσταται."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_GREEK} "Ολοκληρώθηκε"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_GREEK} "Η εγκατάσταση ολοκληρώθηκε επιτυχώς."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_MSGTEXT_ABORTWARNING ${LANG_GREEK} "Είστε σίγουροι πως θέλετε να τερματίσετε την εγκατάσταση του '${NAME}';"
  !endif

  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_BUTTONTEXT_BACK ${LANG_GREEK} "< Πίσω"
    LangString MUI_BUTTONTEXT_NEXT ${LANG_GREEK} "Εμπρός >"
    LangString MUI_BUTTONTEXT_CANCEL ${LANG_GREEK} "’κυρο"
    LangString MUI_BUTTONTEXT_INSTALL ${LANG_GREEK} "Εγκατάσταση"
  !endif

  
  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_GREEK} "Αυτό το πρόγραμμα θα απεγκαταστήσει το '${NAME}' από τον υπολογιστή σας."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_GREEK} "Απεγκατάσταση του '${NAME}'"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_GREEK} "Αφαίρεση του '${NAME}' από τον υπολογιστή σας."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_GREEK} "Απεγκατάσταση Σε Εξέλιξη"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_GREEK} "Παρακαλώ περιμένετε όσο το '${NAME}' απεγκαθίσταται."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_GREEK} "Ολοκληρώθηκε"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_GREEK} "Η απεγκατάσταση ολοκληρώθηκε επιτυχώς."
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_GREEK} "Επόμενο >"
  !endif
 
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_BUTTONTEXT_BACK ${LANG_GREEK} "< Προηγούμενο"
    LangString un.MUI_BUTTONTEXT_NEXT ${LANG_GREEK} "Επόμενο >"
    LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_GREEK} "’κυρο"
    LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_GREEK} "Απεγκατάστ."
  !endif
    
!endif