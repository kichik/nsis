;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: Simplified Chinese (2052)
;By Hotice 

;--------------------------------

!ifndef MUI_SIMPCHINESE_USED

!define MUI_SIMPCHINESE_USED

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_SIMPCHINESE} "按PageDown阅读许可协议的全部内容."
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_SIMPCHINESE} "选取您想安装的组件并取消选取您不想安装的组件. 点击下一步继续." " "
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_SIMPCHINESE} "安装程序将会安装 ${NAME} 至以下的文件夹.$\r$\n$\r$\n确定安装在此文件夹, 点击安装. 欲安装到不同的文件夹, 点击浏览并选取其它文件夹." " "
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_SIMPCHINESE} "下一步 >"
  !endif
   
  LangString MUI_TEXT_LICENSE_TITLE ${LANG_SIMPCHINESE} "许可协议"  
  LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_SIMPCHINESE} "安装 ${NAME} 前请详读许可协议."
  LangString MUI_INNERTEXT_LICENSE ${LANG_SIMPCHINESE} "如果您接受许可协议中的全部条款, 选取我同意继续安装. 您必须接受协议才能安装 ${NAME}."
  
  LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_SIMPCHINESE} "选取组件"
  LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_SIMPCHINESE} "选取 ${NAME} 中您想要安装的功能."
  LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_SIMPCHINESE} "说明"
  LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_SIMPCHINESE} "移动您的光标到任一组件上查看它的说明."
  
  LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_SIMPCHINESE} "选取安装位置"
  LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_SIMPCHINESE} "选取安装 ${NAME} 的文件夹."
  LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_SIMPCHINESE} "目的文件夹"
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_SIMPCHINESE} "安装中"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_SIMPCHINESE} "${NAME} 正在安装中请稍待片刻."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_SIMPCHINESE} "结束"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_SIMPCHINESE} "安装已顺利完成."
  
  LangString MUI_MSGTEXT_ABORTWARNING ${LANG_SIMPCHINESE} "您确定要结束 ${NAME} 安装程序?"

  LangString MUI_BUTTONTEXT_BACK ${LANG_SIMPCHINESE} "< 上一步"
  LangString MUI_BUTTONTEXT_NEXT ${LANG_SIMPCHINESE} "下一步 >"
  LangString MUI_BUTTONTEXT_CANCEL ${LANG_SIMPCHINESE} "取消"
  LangString MUI_BUTTONTEXT_INSTALL ${LANG_SIMPCHINESE} "安装"

  
  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_SIMPCHINESE} "这将会从您的系统中删除 ${NAME}."
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_SIMPCHINESE} "下一步 >"
  !endif
  
  LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_SIMPCHINESE} "删除 ${NAME}"
  LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_SIMPCHINESE} "从您的系统中删除 ${NAME}."
  
  LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_SIMPCHINESE} "删除中"
  LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_SIMPCHINESE} "${NAME} 正在删除中请稍待片刻."
  
  LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_SIMPCHINESE} "结束"
  LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_SIMPCHINESE} "删除已顺利完成."
  
  LangString un.MUI_BUTTONTEXT_BACK ${LANG_SIMPCHINESE} "< 上一步"
  LangString un.MUI_BUTTONTEXT_NEXT ${LANG_SIMPCHINESE} "下一步 >"
  LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_SIMPCHINESE} "取消"
  LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_SIMPCHINESE} "删除"
    
!endif
