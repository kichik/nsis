;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.63

;Language: Thai (1054)
;By TuW@nNu tuwannu@hotmail.com (asdfuae)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "THAI"

  !define MUI_LANGNAME "Thai" ;(ภาษาไทย) Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "ยินดีต้อนรับเข้าสู่การติดตั้งโปรแกรม $(^NameDA) "
  !define MUI_TEXT_WELCOME_INFO_TEXT "ตัวติดตั้งนี้จะติดตั้งโปรแกรม $(^NameDA) ลงบนคอมพิวเตอร์ของคุณ\r\n\r\nทางเราขอแนะนำให้ปิดโปรแกรมอื่นๆให้หมดก่อนที่จะเริ่มติดตั้ง และตัวติดตั้งนี้ไม่ต้องการการรีบูตเครื่องใหม่\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "ข้อตกลงเรื่องลิขสิทธิ์"  
  !define MUI_TEXT_LICENSE_SUBTITLE "กรุณาอ่านข้อตกลงด้านลิขสิทธิ์ก่อนติดตั้งโปรแกรม $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "กดปุ่ม Page Down เพื่ออ่านรายละเอียดที่เหลือ"
  !define MUI_INNERTEXT_LICENSE_BOTTOM "ถ้าคุณยอมรับในข้อตกลงนี้ กรุณากดปุ่ม ยอมรับ และคุณจะต้องตกลงก่อนที่จะเริ่มติดตั้งโปรแกรม $(^NameDA)."
  
  !define MUI_TEXT_COMPONENTS_TITLE "เลือกคอมโพเนนต์"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "เลือก features ของ $(^NameDA) ที่คุณต้องการติดตั้ง"
  !define MUI_INNERTEXT_COMPONENTS_TOP "ใส่เครื่องหมายถูกหน้าสิ่งที่คุณต้องการติดตั้งและเอาเครื่องหมายถูกออกหน้าสิ่งที่คุณไม่ต้องการติดตั้ง"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "รายละเอียด"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "เลื่อนเมาส์ไปบนชื่อคอมโพเนนต์เพื่อดูรายละเอียด"
  
  !define MUI_TEXT_DIRECTORY_TITLE "เลือกที่ที่จะให้ติดตั้ง"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "เลือกโฟลเดอร์ที่ต้องการให้ติดตั้งลง $(^NameDA)."
  !define MUI_INNERTEXT_DIRECTORY_TOP "กำลังที่จะติดตั้งโปรแกรม $(^NameDA) ลงในโฟลเดอร์ด้านล่างนี้$\r$\n$\r$\nหากคุณต้องการให้ติดตั้งลงโฟลเดอร์อื่นกรุณาคลิ๊กปุ่มเรียกหาแล้วเลือกโฟลเดอร์ที่ต้องการ"
  !define MUI_INNERTEXT_DIRECTORY_DESTINATION "โฟลเดอร์จุดหมาย"
  
  !define MUI_TEXT_INSTALLING_TITLE "กำลังติดตั้ง"
  !define MUI_TEXT_INSTALLING_SUBTITLE "กรุณารอสักครู่ระหว่างโปรแกรม $(^NameDA) กำลังติดตั้งอยู่"
  
  !define MUI_TEXT_FINISH_TITLE "ติดตั้งเรียบร้อย"
  !define MUI_TEXT_FINISH_SUBTITLE "การติดตั้งเสร็จสมบูรณ์แล้ว"
  !define MUI_BUTTONTEXT_FINISH "&เสร็จสิ้น"
  !define MUI_TEXT_FINISH_INFO_TITLE "กำลังดำเนินการเรียกโปรแกรมติดตั้ง $(^NameDA) "
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) ถูกติดตั้งเรียบร้อยแล้ว\r\n\r\nคลิ๊กปุ่ม เสร็จสิ้น เพื่อออกจากโปรแกรม"
  !define MUI_TEXT_FINISH_INFO_REBOOT "เครื่องของคุณต้องการการรีบูตหลังจากติดตั้ง $(^NameDA). คุณต้องการที่จะรีบูตเลยหรือไม่"
  !define MUI_TEXT_FINISH_REBOOTNOW "รีบูต"
  !define MUI_TEXT_FINISH_REBOOTLATER "รีบูตทีหลัง"
  !define MUI_TEXT_FINISH_RUN "เปิดโปรแกรม $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "อ่าน Readme"
  
  !define MUI_TEXT_STARTMENU_TITLE "เลือกโฟลเอร์ Start เมนู"
  !define MUI_TEXT_STARTMENU_SUBTITLE "เลือกโฟลเดอร์ Start เมนูที่ต้องการให้ทางลัดของโปรแกรมนี้ไปอยู่"
  !define MUI_INNERTEXT_STARTMENU_TOP "เลือกเมนู Start ที่คุณต้องการให้ทางลัดของโปรแกรมไปอยู่ หรือสร้างโฟลเดอร์ขึ้นมาใหม่"
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "ไม่ต้องสร้างทางลัด"
  
  !define MUI_TEXT_ABORTWARNING "คุณแน่ใจหรือไม่ที่จะออกจากโปรแกรมติดตั้ง $(^Name) "  
  
  
  !define MUI_UNTEXT_CONFIRM_TITLE "ลบโปรแกรม $(^NameDA) ออกจากเครื่อง"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "ลบโปรแกรม $(^NameDA) ออกจากเครื่องของคุณ"
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "กำลังลบ"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "กรุณารอขณะที่ $(^NameDA) กำลังถูกลบ"
    
  !define MUI_UNTEXT_FINISH_TITLE "เรียบร้อย"
  !define MUI_UNTEXT_FINISH_SUBTITLE "การลบโปรแกรมเสร็จสมบูรณ์แล้ว"
  
!insertmacro MUI_LANGUAGEFILE_END