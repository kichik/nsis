;NSIS Modern User Interface - Language File
;version 2 - Compatible with Modern UI 1.4

;Language: Japanese (1041)
;By Dnanako

;--------------------------------
!verbose 3

!ifndef MUI_JAPANESE_USED

!define MUI_JAPANESE_USED

  !define MUI_JAPANESE_LANGNAME "Japanese" ;Name of the language in the language itself (English, Deutsch, Fran軋is etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_JAPANESE} "[Page Down]を押して契約書をすべてお読みください。"
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_JAPANESE} "ライセンス契約書"  
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_JAPANESE} " ${NAME} をインストールする前に、ライセンス条件を確認してください。"
     LangString MUI_INNERTEXT_LICENSE ${LANG_JAPANESE} "${NAME} をインストールするには、契約書に同意する必要があります。$\r$\n契約書のすべての条件に同意するならば、[同意する] を選んでインストールを続けてください。"
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_JAPANESE} "コンポーネントをインストールするにはチェックし、不要ならば、チェックを外してください。 $\r$\n続けるには[次へ]を押してください。"
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_JAPANESE} "コンポーネントを選んでください。"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_JAPANESE} "${NAME} のインストール オプションを選んでください。"
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_JAPANESE} "説明"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_JAPANESE} "コンポーネント上にマウス カーソルを移動すると、ここの説明が表示されます。"
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_JAPANESE} "${NAME} を以下のフォルダにインストールします。$\r$\n$\r$\nこのフォルダにインストールするには、[インストール]を押してください。$\r$\n異なったフォルダにインストールするには、[参照]を押して、別のフォルダを選択してください。" " "
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_JAPANESE} "インストール先を選んでください。"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_JAPANESE} "${NAME} をインストールするフォルダを選んでください。"
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_JAPANESE} "インストール先 フォルダ"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_JAPANESE} "インストール"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_JAPANESE} "${NAME} をインストールしています。しばらくお待ちください。"
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_JAPANESE} "完了"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_JAPANESE} "インストールに成功しました。"
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_JAPANESE} "${NAME} のインストールを中止しますか？"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_JAPANESE} "${NAME} ${VERSION} セットアップ"
  !endif


  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_JAPANESE} "${NAME} をこのシステムからアンインストールします。"
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_JAPANESE} "${NAME} のアンインストール。"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_JAPANESE} "${NAME} をこのシステムからアンインストールします。"
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_JAPANESE} "アンインストール"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_JAPANESE} "${NAME} をアンインストールしています。しばらくお待ちください。"
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_JAPANESE} "完了"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_JAPANESE} "アンインストールに成功しました。"
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_JAPANESE} "${NAME} ${VERSION} セットアップ"
  !endif
    
!endif

!verbose 4