;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4

;Language: 'Chinese (Simplified)' (2052)
;By Kii Ali <kiiali@cpatch.org>

;--------------------------------
!verbose 3

!ifndef MUI_SIMPCHINESE_USED

!define MUI_SIMPCHINESE_USED

  !define MUI_SIMPCHINESE_LANGNAME "Chinese (Simplified)" ;以语言本身的方式，写下语言名称 (English, Deutsch, Fran鏰is etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_SIMPCHINESE} "若要看授权合约的其余部分，请按 [PgDn] 往下卷动页面。"
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_SIMPCHINESE} "授权合约"
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_SIMPCHINESE} "在安装 ${NAME} 之前，请检阅授权条款。"
     LangString MUI_INNERTEXT_LICENSE ${LANG_SIMPCHINESE} "如果你同意所有合约中的条款，选定 [我同意(A)] 继续安装。如果你选定 [取消(C)] ，安装程序将会关闭。必须要接受授权合约才能安装  ${NAME}。"
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_SIMPCHINESE} "核取想要安装的组件，并把不想安装的组件解除核取。按 [下一步(N)] 继续。"
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_SIMPCHINESE} "选定组件"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_SIMPCHINESE} "选定 ${NAME} 中你想要安装的组件。"
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_SIMPCHINESE} "描述"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_SIMPCHINESE} "移动你的鼠标指标到组件之上，便可见到它的描述。"
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_SIMPCHINESE} "安装程序将在下列文件夹，安装 ${NAME} 。$\r$\n$\r$\n要安装到这个文件夹，单击 [安装(I)] 。要安装在不同文件夹，单击 [浏览(B)...] 并选择其他文件夹。"
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_SIMPCHINESE} "选定安装位置" 
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_SIMPCHINESE} "选定 ${NAME} 要安装的文件夹位置。"
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_SIMPCHINESE} "目标文件夹"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_SIMPCHINESE} "正在安装" 
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_SIMPCHINESE} "${NAME} 正在安装，请等候。"
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_SIMPCHINESE} "完成"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_SIMPCHINESE} "安装程序已成功地运行完成。"
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_SIMPCHINESE} "你确实要退出 ${NAME} 安装程序？"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_SIMPCHINESE} "${NAME} ${VERSION} 安装"
  !endif
  

  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_SIMPCHINESE} "这将会从你的系统中解除安装 ${NAME}。"
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_SIMPCHINESE} "解除安装 ${NAME}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_SIMPCHINESE} "从你的系统解除安装 ${NAME} 。"
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_SIMPCHINESE} "正在解除安装"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_SIMPCHINESE} "${NAME} 正在解除安装，请等候。"
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_SIMPCHINESE} "完成"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_SIMPCHINESE} "${NAME} 已从你的系统解除安装。"
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_SIMPCHINESE} "${NAME} ${VERSION} 安装"
  !endif

!endif

!verbose 4