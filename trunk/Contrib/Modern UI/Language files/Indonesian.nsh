;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Indonesian (1057)
;By Ariel825010106@yahoo.com [visit www.ariel106.cjb.net]

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "INDONESIAN"

  !define MUI_LANGNAME "Indonesian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Selamat datang di $(^NameDA) Setup Wizard"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Setup Wizard akan membantu anda pada proses instalasi $(^NameDA).\r\n\r\nSangat disarankan untuk menutup program lainnya sebelum memulai Setup ini. Ini memungkinkan untuk merubah file yang dipakai oleh sistem tanpa harus me-reboot komputer anda.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Perihal Lisensi"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Silahkan membaca lisensi berikut sebelum menginstall $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Tekan Page Down untuk melihat selanjutnya."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Jika anda menerima semua yang ada di lisensi, klik Saya setuju untuk melanjutkan. Anda harus setuju untuk dapat menginstall $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jika anda menerima semua yang ada di lisensi, beri tanda centang. Anda harus setuju untuk dapat menginstall $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jika anda menerima semua yang ada di lisensi, pilihlah salah satu item dibawah ini. Anda harus setuju untuk dapat menginstall $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Pilih Komponen"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Pilih fasilitas dari $(^NameDA) yang ingin di-install."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Deskripsi"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Tunjuk ke salah satu komponen untuk melihat deskripsi tentang komponen itu."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Pilih Lokasi Install"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Pilih folder untuk meng-install $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Install"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Mohon tunggu selama $(^NameDA) sedang di-install."
  
  !define MUI_TEXT_FINISH_TITLE "Instalasi Selesai"
  !define MUI_TEXT_FINISH_SUBTITLE "Setup sudah selesai."
  
  !define MUI_TEXT_ABORT_TITLE "Instalasi Dibatalkan"
  !define MUI_TEXT_ABORT_SUBTITLE "Setup belum selesai secara sempurna."
  
  !define MUI_BUTTONTEXT_FINISH "&Selesai"
  !define MUI_TEXT_FINISH_INFO_TITLE "Menyelesaikan $(^NameDA) Setup Wizard"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) sudah ter-install di komputer anda.\r\n\r\nKlik Selesai untuk menutup Setup Wizard."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Komputer anda harus di reboot untuk menyelesaikan proses instalasi $(^NameDA). Apakah anda mau reboot sekarang juga?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reboot sekarang"
  !define MUI_TEXT_FINISH_REBOOTLATER "Reboot nanti saja"
  !define MUI_TEXT_FINISH_RUN "&Jalankan $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Buka file Readme"
  
  !define MUI_TEXT_STARTMENU_TITLE "Pilih Folder Start Menu"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Pilih folder Start Menu untuk meletakan shortcut $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Pilih folder Start Menu dimana ingin diletakan shortcut program ini. Bisa juga memasukan nama folder yang belum ada untuk membuatnya."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Jangan buat shortcut"
  
  !define MUI_TEXT_ABORTWARNING "Apa anda yakin ingin menghentikan Setup $(^Name)?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Selamat datang di $(^NameDA) Uninstall Wizard"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Uninstall Wizard akan membantu anda pada proses uninstalasi $(^NameDA).\r\n\r\nSebelum memulai uninstalasi, pastikan dulu $(^NameDA) tidak sedang berjalan.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Uninstall $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Menghapus $(^NameDA) dari komputer anda."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Perihal Lisensi"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Silahkan membaca lisensi berikut sebelum meng-uninstall $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Jika anda menerima semua yang ada di lisensi, klik Saya setuju untuk melanjutkan. Anda harus setuju untuk dapat meng-uninstall $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jika anda menerima semua yang ada di lisensi, beri tanda centang. Anda harus setuju untuk dapat meng-uninstall $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jika anda menerima semua yang ada di lisensi, pilihlah salah satu item dibawah ini. Anda harus setuju untuk dapat meng-uninstall $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Pilih Komponen"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Pilih fasilitas dari $(^NameDA) yang ingin di-uninstall."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Pilih Lokasi Uninstall"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Pilih folder untuk meng-uninstall $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Uninstall"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Mohon tunggu selama $(^NameDA) sedang di-uninstall."
    
  !define MUI_UNTEXT_FINISH_TITLE "Proses Uninstall Selesai"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Uninstall sudah selesai."
  
  !define MUI_UNTEXT_ABORT_TITLE "Proses Uninstall Dibatalkan"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Uninstall belum selesai secara sempurna."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Menyelesaikan $(^NameDA) Uninstall Wizard"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) sudah ter-uninstall di komputer anda.\r\n\r\nKlik Selesai untuk menutup Setup Wizard."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Komputer anda harus di reboot untuk menyelesaikan proses uninstall $(^NameDA). Apakah anda mau reboot sekarang juga?"
  
  !define MUI_UNTEXT_ABORTWARNING "Apa anda yakin ingin menghentikan Uninstall $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END