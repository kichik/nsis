;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.63

;Language: Thai (1054)
;By TuW@nNu tuwannu@hotmail.com (asdfuae)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "THAI"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Thai" ;(ภาษาไทย) Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "คลิ๊กปุ่ม ต่อไป เพื่อดำเนินการต่อ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "คลิ๊กปุ่ม ต่อไป เพื่อเริ่มการติดตั้ง"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "ยินดีต้อนรับเข้าสู่การติดตั้งโปรแกรม ${MUI_PRODUCT} "
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "ตัวติดตั้งนี้จะติดตั้งโปรแกรม ${MUI_PRODUCT} ลงบนคอมพิวเตอร์ของคุณ\r\n\r\nทางเราขอแนะนำให้ปิดโปรแกรมอื่นๆให้หมดก่อนที่จะเริ่มติดตั้ง และตัวติดตั้งนี้ไม่ต้องการการรีบูตเครื่องใหม่\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "ข้อตกลงเรื่องลิขสิทธิ์"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "กรุณาอ่านข้อตกลงด้านลิขสิทธิ์ก่อนติดตั้งโปรแกรม ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "กดปุ่ม Page Down เพื่ออ่านรายละเอียดที่เหลือ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "ถ้าคุณยอมรับในข้อตกลงนี้ กรุณากดปุ่ม ยอมรับ และคุณจะต้องตกลงก่อนที่จะเริ่มติดตั้งโปรแกรม ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "เลือกคอมโพเนนต์"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "เลือก features ของ ${MUI_PRODUCT} ที่คุณต้องการติดตั้ง"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "ใส่เครื่องหมายถูกหน้าสิ่งที่คุณต้องการติดตั้งและเอาเครื่องหมายถูกออกหน้าสิ่งที่คุณไม่ต้องการติดตั้ง"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "รายละเอียด"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "เลื่อนเมาส์ไปบนชื่อคอมโพเนนต์เพื่อดูรายละเอียด"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "เลือกที่ที่จะให้ติดตั้ง"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "เลือกโฟลเดอร์ที่ต้องการให้ติดตั้งลง ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "กำลังที่จะติดตั้งโปรแกรม ${MUI_PRODUCT} ลงในโฟลเดอร์ด้านล่างนี้$\r$\n$\r$\nหากคุณต้องการให้ติดตั้งลงโฟลเดอร์อื่นกรุณาคลิ๊กปุ่มเรียกหาแล้วเลือกโฟลเดอร์ที่ต้องการ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "โฟลเดอร์จุดหมาย"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "กำลังติดตั้ง"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "กรุณารอสักครู่ระหว่างโปรแกรม ${MUI_PRODUCT} กำลังติดตั้งอยู่"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "ติดตั้งเรียบร้อย"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "การติดตั้งเสร็จสมบูรณ์แล้ว"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&เสร็จสิ้น"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "กำลังดำเนินการเรียกโปรแกรมติดตั้ง ${MUI_PRODUCT} "
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} ถูกติดตั้งเรียบร้อยแล้ว\r\n\r\nคลิ๊กปุ่ม เสร็จสิ้น เพื่อออกจากโปรแกรม"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "เครื่องของคุณต้องการการรีบูตหลังจากติดตั้ง ${MUI_PRODUCT}. คุณต้องการที่จะรีบูตเลยหรือไม่"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "รีบูต"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "รีบูตทีหลัง"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "เปิดโปรแกรม ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "อ่าน Readme"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "เลือกโฟลเอร์ Start เมนู"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "เลือกโฟลเดอร์ Start เมนูที่ต้องการให้ทางลัดของโปรแกรมนี้ไปอยู่"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "เลือกเมนู Start ที่คุณต้องการให้ทางลัดของโปรแกรมไปอยู่ หรือสร้างโฟลเดอร์ขึ้นมาใหม่"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "ไม่ต้องสร้างทางลัด"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "คุณแน่ใจหรือไม่ที่จะออกจากโปรแกรมติดตั้ง ${MUI_PRODUCT} "  
   
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "คลิ๊กถอนการติดตั้ง เพื่อเริ่มการถอดถอน"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "ลบโปรแกรม ${MUI_PRODUCT} ออกจากเครื่อง"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "ลบโปรแกรม ${MUI_PRODUCT} ออกจากเครื่องของคุณ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "กำลังที่จะลบ ${MUI_PRODUCT} ออกจากเครื่องของคุณ"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "กำลังลบ"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "กรุณารอขณะที่ ${MUI_PRODUCT} กำลังถูกลบ"
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "เรียบร้อย"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "การลบโปรแกรมเสร็จสมบูรณ์แล้ว"
  
!insertmacro MUI_LANGUAGEFILE_END