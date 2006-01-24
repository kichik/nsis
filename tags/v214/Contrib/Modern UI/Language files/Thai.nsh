;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Thai (1054)
;By SoKoOLz, TuW@nNu (asdfuae)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Thai"

  !define MUI_LANGNAME "Thai" ;ใช้เฉพาะตัวอักษร ASCII (ถ้าไม่สามารถทำได้, โปรดใช้ชื่อเป็นภาษาอังกฤษแทน)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "ยินดีต้อนรับเข้าสู่การติดตั้งโปรแกรม $(^NameDA) "
  !define MUI_TEXT_WELCOME_INFO_TEXT "ตัวติดตั้งอัติโนมัติจะนำคุณไปสู่การติดตั้งของ $(^NameDA).\r\n\r\nเราขอแนะนำให้ปิดโปรแกรมอื่นๆให้หมดก่อนที่จะเริ่มติดตั้ง, นี่จะเป็นการอัปเดทไฟล์ได้ง่ายขึ้นโดยคุณไม่จำเป็นต้องทำการรีบูทคอมพิวเตอร์ของคุณ\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "ข้อตกลงเรื่องลิขสิทธิ์"  
  !define MUI_TEXT_LICENSE_SUBTITLE "โปรดอ่านทวนลิขสิทธิ์ในหัวข้อต่างๆอีกครั้งก่อนที่คุณจะทำการติดตั้ง $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "กด Page Down เพื่ออ่านข้อตกลงทั้งหมด"
  !define MUI_INNERTEXT_LICENSE_BOTTOM "ถ้าคุณยอมรับข้อตกลงเรื่องลิขสิทธิ์, กด ฉันยอมรับ เพื่อทำต่อไป, คุณต้องยอมรับในข้อตกลงลิขสิทธิ์เพื่อที่จะทำการติดตั้ง $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "ถ้าคุณยอมรับข้อตกลงเรื่องลิขสิทธ, กดเลือกในกล่องข้างล่างนี้  คุณต้องยอมรับในข้อตกลงลิขสิทธิ์เพื่อที่จะทำการติดตั้ง $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "ถ้าคุณยอมรับข้อตกลงเรื่องลิขสิทธ,  เลือกตัวเลือกแรกด้านล่างนี้ คุณต้องยอมรับในข้อตกลงลิขสิทธิ์เพื่อที่จะทำการติดตั้ง $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "เลือกส่วนประกอบ"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "เลือกสิ่งที่คุณต้องการใช้งานจาก $(^NameDA) ที่คุณต้องการติดตั้ง"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "รายละเอียด"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "ขยับเมาส์ของคุณเหนือส่วนประกอบเพื่อดูรายละเอียด"
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "เลือกส่วนประกอบที่คุณต้องการดูรายละเอียด"
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "เลือกที่ที่ต้องการติดตั้ง"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "เลือกแผ้มที่ต้องการติดตั้ง $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "กำลังติดตั้ง"
  !define MUI_TEXT_INSTALLING_SUBTITLE "โปรดรอในขณะที่ $(^NameDA) กำลังถูกติดตั้ง"
  
  !define MUI_TEXT_FINISH_TITLE "การติดตั้งเสร็จสิ้น"
  !define MUI_TEXT_FINISH_SUBTITLE "การติดตั้งเสร็จสมบูรณ์"
  
  !define MUI_TEXT_ABORT_TITLE "การติดตั้งถูกยกเลิก"
  !define MUI_TEXT_ABORT_SUBTITLE "การติดตั้งไม่เสร็จสมบูรณ์"
  
  !define MUI_BUTTONTEXT_FINISH "&เสร็จสิ้น"
  !define MUI_TEXT_FINISH_INFO_TITLE "การติดตั้งอัติโนมัติของ  $(^NameDA) กำลังเสร็จสิ้น"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) ได้ถูกติดตั้งลงในเครื่องคอมพิวเตอร์ของคุณแล้ว\r\n\r\nกด เสร็จสิ้นเพื่อปิดตัวติดตั้งอัติโนมัติ"
  !define MUI_TEXT_FINISH_INFO_REBOOT "เครื่องคอมพิวเตอร์ของคุณจำเป็นต้องรีสตารท์เพื่อการติดตั้งของ $(^NameDA) จะเรียบร้อย, คุณต้องการจะ รีบูท เดี๋ยวนี้ไหม?"
  !define MUI_TEXT_FINISH_REBOOTNOW "รีบูท เดี๋ยวนี้"
  !define MUI_TEXT_FINISH_REBOOTLATER "ฉันต้องการ รีบูทด้วยตนเอง ทีหลัง"
  !define MUI_TEXT_FINISH_RUN "&รัน $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&แสดงรายละเอียด"
  
  !define MUI_TEXT_STARTMENU_TITLE "เลือกแฟ้ม Start Menu"
  !define MUI_TEXT_STARTMENU_SUBTITLE "เลือกแฟ้ม Start Menu เพื่อสร้างชอร์ตคัทของ $(^NameDA). "
  !define MUI_INNERTEXT_STARTMENU_TOP "เลือกแผ้ม Start Menu ที่คุณต้องการจะสร้างชอร์ตคัทของโปรแกรม, คุณยังสามารถกำหนดชื่อเพื่อสร้างแฟ้มใหม่ได้อีกด้วย"
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "ไม่ต้องสร้าง ชอร์ตคัท"
  
  !define MUI_TEXT_ABORTWARNING "คุณแน่ใจหรือว่าคุณต้องการจะออกจากการติดตั้งของ $(^Name)?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "ยินดีต้อนรับสู่การยกเลิกการติดตั้งอัติโนมัติของ $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "ตัวติดตั้งอัติโนมัตินี้จะนำคุณไปสู่การยกเลิกการติดตั้งของ $(^NameDA).\r\n\r\nการจะเริ่มการยกเลิกการติดตั้งนี้, โปรดตรวจสอบว่า $(^NameDA) ไม่ได้ใช้อยู่\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "ยกเลิกการติดตั้ง $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "ยกเลิกการติดตั้ง $(^NameDA) จากเครื่องคอมพิวเตอร์ของคุณ"
  
  !define MUI_UNTEXT_LICENSE_TITLE "ข้อตกลงเรื่องลิขสิทธิ์" 
  !define MUI_UNTEXT_LICENSE_SUBTITLE "กรุณาอ่านข้อตกลงด้านลิขสิทธิ์ก่อนติดตั้งโปรแกรม $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "ถ้าคุณยอมรับในข้อตกลงนี้ กรุณากดปุ่ม ฉันยอมรับ และคุณจะต้องตกลงก่อนที่จะเริ่มการยกเลิกติดตั้งโปรแกรม $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "ถ้าคุณยอมรับข้อตกลงเรื่องลิขสิทธิ์, กดเลือกในกล่องข้างล่างนี้ คุณต้องยอมรับในข้อตกลงลิขสิทธิ์เพื่อที่จะทำการติดตั้ง $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "ถ้าคุณยอมรับข้อตกลงเรื่องลิขสิทธิ์, เลือกตัวเลือกแรกด้านล่างนี้ คุณต้องยอมรับในข้อตกลงลิขสิทธิ์เพื่อที่จะทำการติดตั้ง $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "เลือกส่วนประกอบ"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE  "เลือกสิ่งที่คุณต้องการใช้งานจาก $(^NameDA) ที่คุณต้องยกเลิกการติดตั้ง"
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "เลือกแฟ้มที่ต้องการยกเลิกการติดตั้ง"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "เลือกแฟ้มที่คุณต้องการยกเลิกการติดตั้งของ $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "กำลังยกเลิกการติดตั้ง"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "โปรดรอในขณะที่ $(^NameDA) กำลังถูกยกเลิกการติดตั้ง."
    
  !define MUI_UNTEXT_FINISH_TITLE "การยกเลิกการติดตั้งเสร็จสิ้น"
  !define MUI_UNTEXT_FINISH_SUBTITLE "การยกเลิกการติดตั้งเสร็จสิ้นโดยสมบูรณ์"
  
  !define MUI_UNTEXT_ABORT_TITLE "การยกเลิกการติดตั้งถูกยกเลิก"
  !define MUI_UNTEXT_ABORT_SUBTITLE "การยกเลิกการติดตั้งไม่สำเร็จ"
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "การยกเลิกการติดตั้งอัติโนมัติของ $(^NameDA) กำลังเสร็จสมบูรณ์"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) ได้ถูกยกเลิกออกจากเครื่องคอมพิวเตอร์ของคุณแล้ว \r\n\r\nกด เสร็จสิ้น เพื่อปิดหน้าจอติดตั้งอัติโนมัติ"
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "เครื่องคอมพิวเตอร์ของคุณจำเป็นต้องรีสตาร์ทในการที่จะทำการยกเลิกการติดตั้งของ $(^NameDA) เสร็จสิ้น, คุณต้องการจะรีบูทเดี๋ยวนี้ไหม?"
  
  !define MUI_UNTEXT_ABORTWARNING "คุณแน่ใจหรือว่าคุณต้องการออกจากการยกเลิกการติดตั้งของ $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END