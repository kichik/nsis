;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: Traditional Chinese (1028)
;By Yi-Ting Cheng (matini_yt@yahoo.com.tw)

;--------------------------------

!ifndef MUI_TRADCHINESE_USED

!define MUI_TRADCHINESE_USED

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_TRADCHINESE} "按 [PageDown] 觀看合約的其餘部分。"
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_TRADCHINESE} "核選您想要安裝的元件，並解除核選您不想要安裝的元件。點選 [下一步] 繼續。"
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_TRADCHINESE} "安裝程式將會安裝 ${NAME} 至下列資料夾。$\r$\n$\r$\n要安裝在此資料夾，點選 [安裝]。要安裝在不同的資料夾，點選 [瀏覽...] 並選擇其它資料夾。" " "
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_TRADCHINESE} "下一步 >"
  !endif
   
  LangString MUI_TEXT_LICENSE_TITLE ${LANG_TRADCHINESE} "授權合約"  
  LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_TRADCHINESE} "在安裝 ${NAME} 之前請檢閱授權條款。"
  LangString MUI_INNERTEXT_LICENSE ${LANG_TRADCHINESE} "如果您同意合約中的所有條款，選取 [我同意] 繼續安裝。您必須接受合約才能安裝 ${NAME}。"
  
  LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_TRADCHINESE} "選取元件"
  LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_TRADCHINESE} "選取 ${NAME} 中您想要安裝的功能。"
  LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_TRADCHINESE} "說明"
  LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_TRADCHINESE} "移動您的滑鼠到元件之上查看它的說明。"
  
  LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_TRADCHINESE} "選取安裝位置"
  LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_TRADCHINESE} "選取安裝 ${NAME} 的資料夾。"
  LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_TRADCHINESE} "目的資料夾"
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_TRADCHINESE} "正在安裝"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_TRADCHINESE} "${NAME} 正在安裝中請稍候。"
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_TRADCHINESE} "完成"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_TRADCHINESE} "安裝已順利完成。"
  
  LangString MUI_MSGTEXT_ABORTWARNING ${LANG_TRADCHINESE} "您確定要結束 ${NAME} 安裝程式?"

  LangString MUI_BUTTONTEXT_BACK ${LANG_TRADCHINESE} "< 上一步"
  LangString MUI_BUTTONTEXT_NEXT ${LANG_TRADCHINESE} "下一步 >"
  LangString MUI_BUTTONTEXT_CANCEL ${LANG_TRADCHINESE} "取消"
  LangString MUI_BUTTONTEXT_INSTALL ${LANG_TRADCHINESE} "安裝"

  
  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_TRADCHINESE} "這將會從您的系統中移除 ${NAME}。"
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_TRADCHINESE} "下一步 >"
  !endif
  
  LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_TRADCHINESE} "移除 ${NAME}"
  LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_TRADCHINESE} "從您的系統中移除 ${NAME}。"
  
  LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_TRADCHINESE} "正在移除"
  LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_TRADCHINESE} "${NAME} 正在移除中請稍候。"
  
  LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_TRADCHINESE} "完成"
  LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_TRADCHINESE} "移除已順利完成。"
  
  LangString un.MUI_BUTTONTEXT_BACK ${LANG_TRADCHINESE} "< 上一步"
  LangString un.MUI_BUTTONTEXT_NEXT ${LANG_TRADCHINESE} "下一步 >"
  LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_TRADCHINESE} "取消"
  LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_TRADCHINESE} "移除"
    
!endif