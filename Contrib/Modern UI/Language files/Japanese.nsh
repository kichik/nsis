;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: Japanese (1041)
;By Dnanako

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "JAPANESE"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Japanese" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "ライセンス契約書"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE " ${MUI_PRODUCT} をインストールする前に、ライセンス条件を確認してください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "[Page Down]を押して契約書をすべてお読みください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "${MUI_PRODUCT} をインストールするには、契約書に同意する必要があります。$\r$\n契約書のすべての条件に同意するならば、[同意する] を選んでインストールを続けてください。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "コンポーネントを選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "${MUI_PRODUCT} のインストール オプションを選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "コンポーネントをインストールするにはチェックし、不要ならば、チェックを外してください。 $\r$\n続けるには[次へ]を押してください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "説明"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "コンポーネント上にマウス カーソルを移動すると、ここの説明が表示されます。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "インストール先を選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "${MUI_PRODUCT} をインストールするフォルダを選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} を以下のフォルダにインストールします。$\r$\n$\r$\nこのフォルダにインストールするには、[インストール]を押してください。$\r$\n異なったフォルダにインストールするには、[参照]を押して、別のフォルダを選択してください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "インストール先 フォルダ"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "インストール"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "${MUI_PRODUCT} をインストールしています。しばらくお待ちください。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "完了"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "インストールに成功しました。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "${MUI_PRODUCT} のインストールを中止しますか？"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "${MUI_PRODUCT} のアンインストール。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "${MUI_PRODUCT} をこのシステムからアンインストールします。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "${MUI_PRODUCT} をこのシステムからアンインストールします。"
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "アンインストール"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "${MUI_PRODUCT} をアンインストールしています。しばらくお待ちください。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "完了"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "アンインストールに成功しました。"
   
!insertmacro MUI_LANGUAGEFILE_END