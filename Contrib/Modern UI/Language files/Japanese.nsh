;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.6

;Language: Japanese (1041)
;By Dnanako

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "JAPANESE"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Japanese" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "続けるには[次へ]を押してください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "[インストール]を押して、インストールを開始してください。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "${MUI_PRODUCT} セットアップ ウィザードへようこそ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "${MUI_PRODUCT} をこのコンピュータにインストールします。\r\n\r\nセットアップを開始する前に、他のすべてのアプリケーションを終了することを推奨します。これは、セットアップがこのシステムを再起動せずに、確実にシステム ファイルをアップデートすることが出来るようになります。\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "ライセンス契約書"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "${MUI_PRODUCT} をインストールする前に、ライセンス条件を確認してください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "[Page Down]を押して契約書をすべてお読みください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "${MUI_PRODUCT} をインストールするには、契約書に同意する必要があります。$\r$\n契約書のすべての条件に同意するならば、[同意する] を選んでインストールを続けてください。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "コンポーネントを選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "${MUI_PRODUCT} のインストール オプションを選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "コンポーネントをインストールするにはチェックし、不要ならば、チェックを外してください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "説明"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "コンポーネント上にマウス カーソルを移動すると、ここの説明が表示されます。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "インストール先を選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "${MUI_PRODUCT} をインストールするフォルダを選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} を以下のフォルダにインストールします。$\r$\n$\r$\n異なったフォルダにインストールするには、[参照]を押して、別のフォルダを選択してください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "インストール先 フォルダ"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "インストール"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "${MUI_PRODUCT} をインストールしています。しばらくお待ちください。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "インストールの完了"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "インストールに成功しました。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE "：完了"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "完了(&F)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "${MUI_PRODUCT} セットアップ ウィザードは完了しました。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} は、このシステムにインストールされました。\r\n\r\nウィザードを閉じるには[完了]を押してください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "${MUI_PRODUCT} のインストールを完了するには、このシステムを再起動する必要があります。今すぐ再起動しますか？"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "今すぐ再起動する"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "後で手動で再起動する"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "${MUI_PRODUCT} を実行。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Readme を表示する。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE "：スタートメニュー フォルダ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "スタートメニュー フォルダを選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "プログラム ショートカットのスタートメニュー フォルダを選んでください。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "プログラム ショートカットを作成したいスタートメニュー フォルダを選択してください。また、作成する新しいフォルダに名前をつけることもできます。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "ショートカットを作成しない"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "${MUI_PRODUCT} のインストールを中止しますか？"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "${MUI_PRODUCT} のアンインストール。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "${MUI_PRODUCT} をこのシステムから削除します。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "${MUI_PRODUCT} をこのシステムからアンインストールします。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "アンインストール"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "${MUI_PRODUCT} をアンインストールしています。しばらくお待ちください。"
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "完了"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "アンインストールに成功しました。"
  
!insertmacro MUI_LANGUAGEFILE_END