;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4

;Language: 'Chinese (Traditional)' (1028)
;By Kii Ali <kiiali@cpatch.org>

;--------------------------------
!verbose 3

!ifndef MUI_TRADCHINESE_USED

!define MUI_TRADCHINESE_USED

  !define MUI_TRADCHINESE_LANGNAME "Chinese (Traditional)" ;以語言本身的方式，寫下語言名稱 (English, Deutsch, Fran蓷is etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_TRADCHINESE} "若要看授權合約的其餘部分，請按 [PgDn] 往下捲動頁面。"
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_TRADCHINESE} "授權合約"
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_TRADCHINESE} "在安裝 ${NAME} 之前，請檢閱授權條款。"
     LangString MUI_INNERTEXT_LICENSE ${LANG_TRADCHINESE} "如果你同意所有合約中的條款，選取 [我同意(A)] 繼續安裝。如果你選取 [取消(C)] ，安裝程式將會關閉。必須要接受授權合約才能安裝  ${NAME}。"
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_TRADCHINESE} "核取想要安裝的元件，並把不想安裝的元件解除核取。按 [下一步(N)] 繼續。"
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_TRADCHINESE} "選取元件"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_TRADCHINESE} "選取 ${NAME} 中你想要安裝的元件。"
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_TRADCHINESE} "描述"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_TRADCHINESE} "移動你的滑鼠指標到元件之上，便可見到它的描述。"
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_TRADCHINESE} "安裝程式將在下列資料夾，安裝 ${NAME} 。$\r$\n$\r$\n要安裝到這個資料夾，按一下 [安裝(I)] 。要安裝在不同資料夾，按一下 [瀏覽(B)...] 並選擇其他資料夾。" " "
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_TRADCHINESE} "選取安裝位置" 
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_TRADCHINESE} "選取 ${NAME} 要安裝的資料夾位置。"
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_TRADCHINESE} "目的資料夾"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_TRADCHINESE} "正在安裝" 
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_TRADCHINESE} "${NAME} 正在安裝，請等候。"
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_TRADCHINESE} "完成"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_TRADCHINESE} "安裝程式已成功地執行完成。"
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_TRADCHINESE} "你確定要離開 ${NAME} 安裝程式？"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_TRADCHINESE} "${NAME} ${VERSION} 安裝"
  !endif


  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_TRADCHINESE} "這將會從你的系統中解除安裝 ${NAME}。"
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_TRADCHINESE} "解除安裝 ${NAME}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_TRADCHINESE} "從你的系統解除安裝 ${NAME} 。"
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_TRADCHINESE} "正在解除安裝"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_TRADCHINESE} "${NAME} 正在解除安裝，請等候。"
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_TRADCHINESE} "完成"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_TRADCHINESE} "${NAME} 已從你的系統解除安裝。"
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_TRADCHINESE} "${NAME} ${VERSION} 安裝"
  !endif
    
!endif

!verbose 4