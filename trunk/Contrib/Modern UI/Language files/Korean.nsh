;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Korean (1042)
;By linak linak@korea.com / modified by koder@popdesk.co.kr 수정시 메일로 '꼭' 연락 바랍니다.

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "KOREAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Korean"  ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "$(^Name) 설치 마법사에 오신 것을 환영합니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "이 마법사는 당신의 컴퓨터에 $(^Name)(을)를 설치할 것입니다.\r\n\r\n설치를 시작하기 전에 모든 프로그램을 종료해주시기 바랍니다. 프로그램을 종료하게 되면 시스템을 재부팅 하지 않고서도 특정한 시스템 파일을 수정할 수 있기 때문입니다.\r\n\r\n$_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "사용 계약 동의"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "$(^Name)(을)를 설치하시기 전에 사용 계약 내용을 살펴보시기 바랍니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "사용 계약 동의 사항의 나머지 부분을 보시려면 [Page Down] 키를 눌러 주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "내용에 동의 하셨다면 '동의함'을 눌러 주세요. $(^Name)(을)를 설치하기 위해서는 반드시 내용에 동의하셔야 합니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "내용에 동의 하셨다면 아래 사항을 선택해 주세요. $(^Name)(을)를 설치하기 위해서는 반드시 내용에 동의하셔야 합니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "내용에 동의 하셨다면 첫번째 사항을 선택해 주세요. $(^Name)(을)를 설치하기 위해서는 반드시 내용에 동의하셔야 합니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "구성 요소 선택"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "설치하고자 하는 $(^Name)의 구성 요소를 선택해 주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "상세 설명"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "상세 설명을 보시고 싶으신 부분에 마우스를 올려놓으세요."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "설치 위치 선택"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "$(^Name)(을)를 설치 할 폴더를 선택해 주세요."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "설치중"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "$(^Name)(을)를 설치하는 동안 잠시 기다려 주세요."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "설치 완료"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "설치가 성공적으로 이루어졌습니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "설치 취소"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "설치가 취소 되었습니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "마침"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "$(^Name) 설치 마법사 완료"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "$(^Name)의 설치가 완료 되었습니다. 설치 마법사를 마치려면 '마침' 버튼을 눌러 주세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "$(^Name)의 설치를 완료하기 위해서는 시스템을 다시 시작해야 합니다. 지금 재부팅 하시겠습니까?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "지금 재부팅 하겠습니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "나중에 재부팅 하겠습니다."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "$(^Name) 실행하기(&R)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Readme 파일 보기(&S)"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "시작 메뉴 폴더 선택"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "프로그램의 바로가기 아이콘이 생성될 시작 메뉴 폴더 선택."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "프로그램의 바로가기 아이콘이 생성될 시작 메뉴 폴더를 선택하세요. 새로운 폴더를 생성하려면 폴더 이름을 입력하세요."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "바로 가기 아이콘을 만들지 않겠습니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "$(^Name) 설치를 종료 하시겠습니까?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "$(^Name) 제거"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "시스템에서 $(^Name) 제거하기"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "구성 요소 선택"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "제거하고자 하는 $(^Name)의 구성 요소를 선택해 주세요."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "제거중"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "$(^Name)(을)를 제거하는 동안 잠시 기다려 주시기 바랍니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "제거 마침"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "프로그램을 성공적으로 제거하였습니다."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "프로그램 제거 취소"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "프로그램 제거가 취소 되었습니다."

!insertmacro MUI_LANGUAGEFILE_END