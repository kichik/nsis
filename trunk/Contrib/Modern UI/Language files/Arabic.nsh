;Modern UI „·› ·€…
;«·≈’œ«— «·√Ê· - Ì⁄„· „⁄ Modern UI 1.3

;Language: Arabic (1025)
;By asdfuae@msn.com

;--------------------------------

!ifndef MUI_ARABIC_USED

!define MUI_ARABIC_USED

  !define MUI_ARABIC_LANGNAME "⁄—»Ì" ;Name of the language in the language itself (Arabic, Deutsch, FranÁais etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_ARABIC} "≈÷€ÿ «”›· «·’›Õ… ·ﬁ—¬Â « ›«ﬁÌ…˛ «· —ŒÌ’"
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_ARABIC} "« ›«ﬁÌ…˛ «· —ŒÌ’"  
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_ARABIC} "«·—Ã«¡ „—«Ã⁄… « ›«ﬁÌ…˛ «· —ŒÌ’ ﬁ»·  ‰’Ì» ${NAME}."
     LangString MUI_INNERTEXT_LICENSE ${LANG_ARABIC} "≈÷€ÿ „Ê«›ﬁ · ﬂ„·… «· ‰’Ì» ≈‰ Ê«›ﬁ  ⁄·Ï ﬂ«„· « ›«ﬁÌ…˛ «· —ŒÌ’° ·«  ” ÿÌ⁄ «· ‰’Ì» »œÊ‰ «·„Ê«›ﬁ… ⁄·Ï « ›«ﬁÌ…˛  —ŒÌ’ ${NAME}."
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_ARABIC} "√Œ — «·⁄‰«’— «·„—«œ  ‰’Ì»Â« Ê √“· «·⁄‰«’— «·€Ì— „—€Ê»…° À„ √÷€ÿ «· «·Ì · ﬂ„·… «· ‰’Ì»"
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_ARABIC} "√Œ — «·⁄‰«’— ·· ‰’Ì»"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_ARABIC} "√Œ — «·„Ì“ «·„—«œ  ‰’Ì»Â« „‰ ${NAME}."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_ARABIC} "«·Ê’›"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_ARABIC} "√‘— »›√—ﬂ ⁄·Ï √Õœ «·⁄‰«’— ·„⁄—›… „Ì“ Â √Ê Ê’›Â"
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_ARABIC} "”Ì „  ‰’Ì» «·»—‰«„Ã ${NAME} ›Ì «·„Ã·œ «· «·Ì$\r$\n$\r$\n≈÷€ÿ  ‰’Ì» · ‰’Ì»Â ›Ì Â–« «·„Ã·œ. · ‰’Ì»Â ›Ì „Ã·œ ¬Œ—° ≈÷€ÿ  ’›Õ Ê √Œ — «·„Ã·œ «·¬Œ—." " "
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_ARABIC} "√Œ — „Êﬁ⁄ «· ‰’Ì»"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_ARABIC} "√Œ — «·„Ã·œ «·„—«œ  ‰’Ì» ›ÌÂ «·»—‰«„Ã ${NAME}."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_ARABIC} "«·„Ã·œ «·„Œ’’"
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_ARABIC} "«· «·Ì >"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_ARABIC} "Ìı‰’¯ˆ»˛"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_ARABIC} "«·—Ã«¡ «·«‰ Ÿ«— ·ÕÌ‰  ‰’Ì» ${NAME}."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_ARABIC} "«·Õ„œ··Â  „ »‰Ã«Õ"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_ARABIC} "«·Õ„œ··Â  „ »‰Ã«Õ  ‰’Ì» «·»—‰«„Ã"
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_MSGTEXT_ABORTWARNING ${LANG_ARABIC} "Â· √‰  „ √ﬂœ „‰ ≈€·«ﬁ „‰’» ${NAME}ø"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_BUTTONTEXT_BACK ${LANG_ARABIC} "< «·”«»ﬁ"
    LangString MUI_BUTTONTEXT_NEXT ${LANG_ARABIC} "«· «·Ì >"
    LangString MUI_BUTTONTEXT_CANCEL ${LANG_ARABIC} "≈·€«¡"
    LangString MUI_BUTTONTEXT_INSTALL ${LANG_ARABIC} " ‰’Ì»"
  !endif


  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_ARABIC} "Â–« ”Ì“Ì· „‰ ‰Ÿ«„ﬂ «·»—‰«„Ã ${NAME}"
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_ARABIC} "„“Ì· ${NAME}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_ARABIC} "≈“«·… ${NAME} „‰ ‰Ÿ«„ﬂ"
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_ARABIC} "≈“«·…"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_ARABIC} "«·—Ã«¡ «·«‰ Ÿ«— ·ÕÌ‰ ≈“«·… ${NAME}."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_ARABIC} " „ "
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_ARABIC} " „  «·≈“«·… »‰Ã«Õ Ê «·Õ„œ··Â"
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_ARABIC} "«· «·Ì& >"
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_BUTTONTEXT_BACK ${LANG_ARABIC} "< «·”«»ﬁ"
    LangString un.MUI_BUTTONTEXT_NEXT ${LANG_ARABIC} "«· «·Ì >"
    LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_ARABIC} "≈·€«¡"
    LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_ARABIC} "≈“«·…&"
  !endif  
    
!endif