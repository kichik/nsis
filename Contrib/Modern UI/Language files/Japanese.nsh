;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: Japanese (1041)
;By Dnanako

;Translation updated by Takahiro Yoshimura <takahiro_y@monolithworks.co.jp>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "JAPANESE"

  !define MUI_LANGNAME "Japanese" ;(日本語) Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "$(^Name) セットアップ ウィザードへようこそ"
  !define MUI_TEXT_WELCOME_INFO_TEXT "このウィザードは、$(^Name)のインストールをガイドしていきます。\r\n\r\nセットアップを開始する前に、他のすべてのアプリケーションを終了することを推奨します。これによって、セットアップがこのコンピュータを再起動せずに、確実にシステム ファイルをアップデートすることが出来るようになります。\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "ライセンス契約書"  
  !define MUI_TEXT_LICENSE_SUBTITLE "$(^Name)をインストールする前に、ライセンス条件を確認してください。"
  !define MUI_INNERTEXT_LICENSE_TOP "[Page Down]を押して契約書をすべてお読みください。"
  !define MUI_INNERTEXT_LICENSE_BOTTOM "契約書のすべての条件に同意するならば、[同意する] を選んでインストールを続けてください。$(^Name) をインストールするには、契約書に同意する必要があります。"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "契約書のすべての条件に同意するならば、下のチェックボックスをクリックしてください。$(^Name) をインストールするには、契約書に同意する必要があります。 $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "契約書のすべての条件に同意するならば、下に表示されているオプションのうち、最初のものを選んで下さい。$(^Name) をインストールするには、契約書に同意する必要があります。 $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "コンポーネントを選んでください。"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "$(^Name)のインストール オプションを選んでください。"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "説明"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "コンポーネントの上にマウス カーソルを移動すると、ここに説明が表示されます。"
  
  !define MUI_TEXT_DIRECTORY_TITLE "インストール先を選んでください。"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "$(^Name)をインストールするフォルダを選んでください。"

  !define MUI_TEXT_INSTALLING_TITLE "インストール"
  !define MUI_TEXT_INSTALLING_SUBTITLE "$(^Name)をインストールしています。しばらくお待ちください。"

  !define MUI_TEXT_FINISH_TITLE "インストールの完了"
  !define MUI_TEXT_FINISH_SUBTITLE "インストールに成功しました。"

  !define MUI_TEXT_ABORT_TITLE "インストールの中止"
  !define MUI_TEXT_ABORT_SUBTITLE "セットアップは正常に完了されませんでした。"
  
  !define MUI_BUTTONTEXT_FINISH "完了(&F)"
  !define MUI_TEXT_FINISH_INFO_TITLE "$(^Name) セットアップ ウィザードは完了しました。"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name)は、このコンピュータにインストールされました。\r\n\r\nウィザードを閉じるには [完了] を押してください。"
  !define MUI_TEXT_FINISH_INFO_REBOOT "$(^Name) のインストールを完了するには、このコンピュータを再起動する必要があります。今すぐ再起動しますか？"
  !define MUI_TEXT_FINISH_REBOOTNOW "今すぐ再起動する"
  !define MUI_TEXT_FINISH_REBOOTLATER "後で手動で再起動する"
  !define MUI_TEXT_FINISH_RUN "$(^Name)を実行(&R)"
  !define MUI_TEXT_FINISH_SHOWREADME "Readme を表示する(&S)"
  
  !define MUI_TEXT_STARTMENU_TITLE "スタートメニュー フォルダを選んでください。"
  !define MUI_TEXT_STARTMENU_SUBTITLE "$(^Name)のショートカットを作成するスタートメニュー フォルダを選んで下さい。"
  !define MUI_INNERTEXT_STARTMENU_TOP "このプログラムのショートカットを作成したいスタートメニュー フォルダを選択してください。また、作成する新しいフォルダに名前をつけることもできます。"
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "ショートカットを作成しない"
  
  !define MUI_TEXT_ABORTWARNING "$(^Name) セットアップを中止しますか？"  
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "$(^Name) アンインストール ウィザードへようこそ"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "このウィザードは、$(^Name)のアンインストールをガイドしていきます。\r\n\r\nアンインストールを開始する前に、$(^Name)が起動していないことを確認して下さい。\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "$(^Name)のアンインストール"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "$(^Name)をこのコンピュータから削除します。"

  !define MUI_UNTEXT_LICENSE_TITLE "ライセンス契約書"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "$(^Name)をアンインストールする前に、ライセンス条件を確認してください。"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "契約書のすべての条件に同意するならば、[同意する] を選んでアンインストールを続けてください。$(^Name) をアンインストールするには、契約書に同意する必要があります。"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "契約書のすべての条件に同意するならば、下のチェックボックスをクリックしてください。$(^Name) をアンインストールするには、契約書に同意する必要があります。 $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "契約書のすべての条件に同意するならば、下に表示されているオプションのうち、最初のものを選んで下さい。$(^Name) をアンインストールするには、契約書に同意する必要があります。 $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "コンポーネントを選んでください。"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "$(^Name)のアンインストール オプションを選んでください。"
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "アンインストール元を選んでください。"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "$(^Name)をアンインストールするフォルダを選んでください。"
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "アンインストール"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "$(^Name)をアンインストールしています。しばらくお待ちください。"
  
  !define MUI_UNTEXT_FINISH_TITLE "アンインストールの完了"
  !define MUI_UNTEXT_FINISH_SUBTITLE "アンインストールに成功しました。"
  
  !define MUI_UNTEXT_ABORT_TITLE "アンインストールの中止"
  !define MUI_UNTEXT_ABORT_SUBTITLE "アンインストールは正常に完了されませんでした。"

  !define MUI_UNTEXT_FINISH_INFO_TITLE "$(^Name) アンインストール ウィザードは完了しました。"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name)は、このコンピュータからアンインストールされました。\r\n\r\nウィザードを閉じるには [完了] を押してください。"
  
  !define MUI_UNTEXT_ABORTWARNING "$(^Name) アンインストールを中止しますか？"  
  
!insertmacro MUI_LANGUAGEFILE_END