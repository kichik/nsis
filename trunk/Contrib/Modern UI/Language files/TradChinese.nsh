;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.64

;Language: 'Chinese (Traditional)' (1028)
;Kii Ali <kiiali@cpatch.org>
;Revision date: 2003-03-29
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "TRADCHINESE"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Chinese (Traditional)" ;(以語言本身的方式，寫下語言名稱) Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "按一下 [下一步(N)] 繼續。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "按一下 [安裝(I)] 開始安裝。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "歡迎使用 ${MUI_PRODUCT} 安裝精靈"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "這將會在你的電腦，安裝 ${MUI_PRODUCT} 。\r\n\r\n在開始安裝之前，建議先關閉其他所有應用程式。這將允許\「安裝程式」更新特定的系統檔案，而不需要重新啟動你的電腦。\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "授權合約"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "在安裝 ${MUI_PRODUCT} 之前，請檢閱授權條款。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "檢閱授權合約的其餘部分，請按 [PgDn] 往下捲動頁面。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "如果你同意所有合約中的條款，按一下 [我同意(I)] 繼續安裝。如果你選取 [取消(C)] ，安裝程式將會關閉。必須要接受授權合約才能安裝 ${MUI_PRODUCT} 。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "如果你接受授權合約中的條款，按一下下方的核取方塊。必須要接受授權合約才能安裝 ${MUI_PRODUCT}。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "如果你接受授權合約中的條款，選擇下方第一個選項。必須要接受授權合約才能安裝 ${MUI_PRODUCT}。"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "選取元件"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "選取 ${MUI_PRODUCT} 當中你想要安裝的功能。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "核取想要安裝的元件，並把不想安裝的元件解除核取。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "描述"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "移動你的滑鼠指標到元件之上，便可見到它的描述。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "選取安裝位置" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "選取 ${MUI_PRODUCT} 要安裝的資料夾。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "安裝程式將在下列資料夾，安裝 ${MUI_PRODUCT} 。$\r$\n$\r$\n要安裝到這個資料夾，按一下 [安裝(I)] 。要安裝在不同資料夾，按一下 [瀏覽(B)...] 並選擇其他資料夾。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "目的資料夾"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "正在安裝"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "${MUI_PRODUCT} 正在安裝，請等候。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "安裝完成"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "安裝程式已成功地執行完成。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "完成(&F)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "完成 ${MUI_PRODUCT} 安裝精靈"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} 已在你的系統安裝。\r\n按一下 [完成(F)] 關閉此精靈。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "你的系統需要重新啟動，以便完成 ${MUI_PRODUCT} 的安裝。現在要重新啟動嗎？"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "現在重新啟動(&N)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "稍後手動地重新啟動(&L)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "執行 ${MUI_PRODUCT}(&R)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "顯示「讀我」(&M)"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "選擇開始功能表資料夾"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "選擇「開始功能表」資料夾，用於程式的捷徑。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "選擇「開始功能表」資料夾，以便建立程式的捷徑。你也可以輸入名稱，建立新資料夾。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "不要建立捷徑(&N)"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "你確定要離開 ${MUI_PRODUCT} 安裝程式？"
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "按一下 [移除(U)] 按鈕，開始解除安裝。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "解除安裝 ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "從你的電腦解除安裝 ${MUI_PRODUCT} 。"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "這個精靈將從你的電腦解除安裝 ${MUI_PRODUCT} 。"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "正在解除安裝"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "${MUI_PRODUCT} 正在解除安裝，請等候。"
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "已完成"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "解除安裝程式已成功地執行完成。"
  
!insertmacro MUI_LANGUAGEFILE_END