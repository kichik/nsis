;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: 'Chinese (Traditional)' (1028)
;Translator: Kii Ali <kiiali@cpatch.org>
;Revision date: 2003-10-23
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "TRADCHINESE"

  !define MUI_LANGNAME "Chinese (Traditional)" ;(以語言本身的方式，寫下語言名稱) Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_FONT "新細明體"
  !define MUI_FONTSIZE "9"
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "歡迎使用 $(^Name) 安裝精靈"
  !define MUI_TEXT_WELCOME_INFO_TEXT "這將會在你的電腦，安裝 $(^Name) 。\r\n\r\n在開始安裝之前，建議先關閉其他所有應用程式。這將允許\「安裝程式」更新指定的系統檔案，而不需要重新啟動你的電腦。\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "授權協議"
  !define MUI_TEXT_LICENSE_SUBTITLE "在安裝 $(^Name) 之前，請檢閱授權條款。"
  !define MUI_INNERTEXT_LICENSE_TOP "檢閱協議的其餘部分，請按 [PgDn] 往下捲動頁面。"
  !define MUI_INNERTEXT_LICENSE_BOTTOM "如果你接受協議中的條款，按一下 [我同意(I)] 繼續安裝。如果你選取 [取消(C)] ，安裝程式將會關閉。必須要接受協議才能安裝 $(^Name) 。"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "如果你接受協議中的條款，按一下下方的勾選框。必須要接受協議才能安裝 $(^Name)。$_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "如果你接受協議中的條款，選擇下方第一個選項。必須要接受協議才能安裝 $(^Name)。$_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "選取元件"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "選取 $(^Name) 當中你想要安裝的功能。"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "描述"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "移動你的滑鼠指標到元件之上，便可見到它的描述。"
  
  !define MUI_TEXT_DIRECTORY_TITLE "選取安裝位置" 
  !define MUI_TEXT_DIRECTORY_SUBTITLE "選取 $(^Name) 要安裝的資料夾。"

  !define MUI_TEXT_INSTALLING_TITLE "正在安裝"
  !define MUI_TEXT_INSTALLING_SUBTITLE "$(^Name) 正在安裝，請等候。"
  
  !define MUI_TEXT_FINISH_TITLE "安裝完成"
  !define MUI_TEXT_FINISH_SUBTITLE "安裝程式已成功地執行完成。"
  
  !define MUI_TEXT_ABORT_TITLE "安裝己中止"
  !define MUI_TEXT_ABORT_SUBTITLE "安裝程式並未成功地執行完成。"
  
  !define MUI_BUTTONTEXT_FINISH "完成(&F)"
  !define MUI_TEXT_FINISH_INFO_TITLE "完成 $(^Name) 安裝精靈"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) 已在你的系統安裝。\r\n按一下 [完成(F)] 關閉此精靈。"
  !define MUI_TEXT_FINISH_INFO_REBOOT "你的系統需要重新啟動，以便完成 $(^Name) 的安裝。現在要重新啟動嗎？"
  !define MUI_TEXT_FINISH_REBOOTNOW "是，現在重新啟動(&Y)"
  !define MUI_TEXT_FINISH_REBOOTLATER "否，我稍後再自行重新啟動(&N)"
  !define MUI_TEXT_FINISH_RUN "執行 $(^Name)(&R)"
  !define MUI_TEXT_FINISH_SHOWREADME "顯示「讀我檔案」(&M)"
  
  !define MUI_TEXT_STARTMENU_TITLE "選擇「開始功能表」資料夾"
  !define MUI_TEXT_STARTMENU_SUBTITLE "選擇「開始功能表」資料夾，用於程式的捷徑。"
  !define MUI_INNERTEXT_STARTMENU_TOP "選擇「開始功能表」資料夾，以便建立程式的捷徑。你也可以輸入名稱，建立新資料夾。"
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "不要建立捷徑(&N)"
  
  !define MUI_TEXT_ABORTWARNING "你確定要離開 $(^Name) 安裝程式？"
  

  !define MUI_UNTEXT_WELCOME_INFO_TITLE "歡迎使用 $(^Name) 解除安裝精靈"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "這個精靈將全程指引你 $(^Name) 的解除安裝進程。\r\n\r\n在開始解除安裝之前，確認 $(^Name) 並未執行當中。\r\n\r\n$_CLICK"
 
  !define MUI_UNTEXT_CONFIRM_TITLE "解除安裝 $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "從你的電腦解除安裝 $(^Name) 。"
  
  !define MUI_UNTEXT_LICENSE_TITLE "授權協議"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "在解除安裝 $(^Name) 之前，請檢閱授權條款。"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "如果你接受協議中的條款，按一下 [我同意(I)] 繼續解除安裝。如果你選取 [取消(C)] ，安裝程式將會關閉。必須要接受協議才能解除安裝 $(^Name) 。"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "如果你接受協議中的條款，按一下下方的勾選框。必須要接受協議才能解除安裝 $(^Name)。$_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "如果你接受協議中的條款，選擇下方第一個選項。必須要接受協議才能解除安裝 $(^Name)。$_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "選取元件"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "選取 $(^Name) 當中你想要解除安裝的功能。"
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "選取解除安裝位置" 
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "選取 $(^Name) 要解除安裝的資料夾。"

  !define MUI_UNTEXT_UNINSTALLING_TITLE "正在解除安裝"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "$(^Name) 正在解除安裝，請等候。"
    
  !define MUI_UNTEXT_FINISH_TITLE "解除安裝已完成"
  !define MUI_UNTEXT_FINISH_SUBTITLE "解除安裝程式已成功地執行完成。"
  
  !define MUI_UNTEXT_ABORT_TITLE "解除安裝已中止"
  !define MUI_UNTEXT_ABORT_SUBTITLE "解除安裝程式並未成功地執行完成。"
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "正在完成 $(^Name) 解除安裝精靈"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) 已從你的電腦解除安裝。\r\n\r\n按一下 [完成] 關閉這個精靈。"
  
  !define MUI_UNTEXT_ABORTWARNING "你確定要離開 $(^Name) 解除安裝嗎？"  
  
!insertmacro MUI_LANGUAGEFILE_END