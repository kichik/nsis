;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.61

;Language: Korea (1042)
;By Hobin linak@korea.com

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "KOREAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Korean"; 한글 Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "계속 하시려면 다음 버튼을 눌러주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "설치를 시작하시려면 설치 버튼을 눌러주세요."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_TITLE "${MUI_PRODUCT} 설치 마법사에 오신 것을 환영합니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO "이 마법사는 당신의 컴퓨터에 ${MUI_PRODUCT}를 설치할 것입니다.\r\n\r\n설치를 시작하기 전에 모든 프로그램을 종료해주시기 바랍니다. 프로그램을 종료하게 되면 시스템을 재부팅 하지 않고서도 특정한 시스템 파일을 수정할 수 있기 때문입니다.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "사용 계약 동의"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "${MUI_PRODUCT}를 설치하시기 전에 사용 계약 내용을 살펴보시기 바랍니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "사용 계약 동의 사항의 나머지 부분을 보시려면 [Page Down] 키를 눌러주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "내용에 동의 하셨다면 동의함을 선택해 주세요. ${MUI_PRODUCT}를 설치하기 위해서는 반드시 내용에 동의하셔야 합니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "구성 요소 선택"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "설치하고자 하는 ${MUI_PRODUCT}의 구성 요소를 선택해 주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "설치하고 싶으신 부분은 선택해 주시고, 설치하고 싶지 않으신 부분은 선택해제 해주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "상세 설명"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "상세 설명을 보시고 싶으신 부분에 마우스를 올려놓으세요."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "설치 위치 선택"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE " ${MUI_PRODUCT}를 설치 할 폴더를 선택해 주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT}를 다음의 폴더에 설치할 예정입니다. $\r$\n$\r$\n다른 폴더에 설치하고 싶으시면 Browse 버튼을 눌러서 다른 폴더를 선택해주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "설치할 폴더"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "설치"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "${MUI_PRODUCT}를 설치하는 동안 잠시 기다려 주세요."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "설치 완료"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "설치가 성공적으로 이루어졌습니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": 마침"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&마침"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT}의 설치가 완료 되었습니다. 설치 마법사를 마치려면 마침 버튼을 눌러주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "${MUI_PRODUCT}의 설치를 완전히 하기 위해서는 시스템을 다시 시작해야 합니다. 지금 재부팅 할까요?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "지금 재부팅 하겠습니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "나중에 재부팅 하겠습니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN " ${MUI_PRODUCT} 실행하기"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Readme 파일 보기"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": 시작 메뉴 폴더"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "시작 메뉴 폴더 선택."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "프로그램의 바로가기 아이콘이 들어갈 시작 메뉴 폴더 선택"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "프로그램의 바로 가기 아이콘을 넣을 시작 메뉴 폴더를 선택하세요. 새로운 폴더를 생성하려면 폴더 이름을 입력하세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "바로 가기 만들지 않겠습니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "${MUI_PRODUCT} 설정을 정말로 종료겠습니까?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "${MUI_PRODUCT} 삭제"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE " 시스템에서 ${MUI_PRODUCT} 제거하기"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "시스템에서 ${MUI_PRODUCT}를 삭제 할 것입니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "삭제중"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "${MUI_PRODUCT}를 삭제하는 동안 잠시 기다려 주시기 바랍니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "마침"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "프로그램을 성공적으로 삭제하였습니다."

!insertmacro MUI_LANGUAGEFILE_END