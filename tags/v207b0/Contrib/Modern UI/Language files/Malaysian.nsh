;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Malaysian (1086)
;By muhammadazwa@yahoo.com

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Malaysian"

  !define MUI_LANGNAME "Malaysian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Assalamualaikum, Selamat datang ke $(^NameDA) Setup Wizard"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Setup Wizard akan membantu anda untuk memasukkan $(^NameDA).\r\n\r\nSila tutup program aplikasi yang lain sebelum Setup ini dimulakan. Ini supaya tiada proses reboot komputer diperlukan.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Perlesenan"
  !define MUI_TEXT_LICENSE_SUBTITLE "Sila baca teks lesen berikut sebelum memasukkan $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Tekan Page Down untuk melihat teks selebihnya."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Jika anda bersetuju, klik Saya setuju untuk teruskan. Anda mesti setuju untuk sebelum aplikasi dapat dimasukkan $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jika anda bersetuju dengan syarat-syarat lesen, sila tanda dicheckbox. Anda mesti setuju sebelum memasukkan $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jika anda terima semua yang ada di lesen, pilihlah salah satu item dibawah ini. Anda mesti setuju sebelum memasukkan $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Pilih Komponen"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Pilih fungsi-fungsi dari $(^NameDA) yang ingin dimasukkan."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Penerangan"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Alihkan tetikus ke komponen untuk mengetahui penerangannya."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pilih komponen untuk mengetahui penerangannya."
  !endif

  !define MUI_TEXT_DIRECTORY_TITLE "Pilih Lokasi Kemasukan"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Pilih folder untuk memasukkan $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Install"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Sila tunggu ketika $(^NameDA) sedang dimasukkan."

  !define MUI_TEXT_FINISH_TITLE "Proses Selesai"
  !define MUI_TEXT_FINISH_SUBTITLE "Setup sudah selesai."

  !define MUI_TEXT_ABORT_TITLE "Proses Dibatalkan"
  !define MUI_TEXT_ABORT_SUBTITLE "Setup terbatal."

  !define MUI_BUTTONTEXT_FINISH "&Selesai"
  !define MUI_TEXT_FINISH_INFO_TITLE "Menyelesaikan $(^NameDA) Setup Wizard"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) sudah dimasukkan di komputer anda.\r\n\r\nKlik Selesai untuk menutup Setup Wizard."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Komputer anda harus di reboot untuk menyelesaikan proses memasukkan $(^NameDA). Apakah anda hendak reboot sekarang juga?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reboot sekarang"
  !define MUI_TEXT_FINISH_REBOOTLATER "Reboot nanti"
  !define MUI_TEXT_FINISH_RUN "&Jalankan $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Buka file Readme"

  !define MUI_TEXT_STARTMENU_TITLE "Pilih Folder Start Menu"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Pilih folder Start Menu untuk meletakkan shortcut $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Pilih folder Start Menu untuk perletakkan shortcut aplikasi ini. Boleh cipta nama folder anda sendiri."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Tidak perlu shortcut"

  !define MUI_TEXT_ABORTWARNING "Adakan anda yakin ingin membatalkan Setup $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Selamat datang ke $(^NameDA) Uninstall Wizard"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Uninstall Wizard akan membantu anda pada proses membuang $(^NameDA).\r\n\r\nSebelum membuang, pastikan dulu $(^NameDA) dimatikan.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Buang $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Padam $(^NameDA) dari komputer anda."

  !define MUI_UNTEXT_LICENSE_TITLE "Tentang Lesen"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Sila baca teks lesen sebelum membuang $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Jika anda menerima lesen, klik Saya setuju untuk teruskan. Anda mesti setuju untuk dapat membuang $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jika anda menerima semua yang ada di lesen, beri tanda dicheckbox. Anda mesti setuju untuk dapat membuang $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jika anda menerima semua yang ada di lesen, pilihlah salah satu item dibawah ini. Anda mesti setuju untuk dapat membuang $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Pilih Komponen"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Pilih fungsi-fungsi $(^NameDA) yang ingin dibuang."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Pilih Lokasi Uninstall"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Pilih folder untuk meng-uninstall $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Uninstall"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Sila tunggu ketika $(^NameDA) sedang di-buang."

  !define MUI_UNTEXT_FINISH_TITLE "Proses Uninstall Selesai"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Uninstall sudah selesai."

  !define MUI_UNTEXT_ABORT_TITLE "Proses Uninstall Dibatalkan"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Uninstall belum selesai secara sempurna."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Menyelesaikan $(^NameDA) Uninstall Wizard"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) sudah dibuang dari komputer anda.\r\n\r\nKlik Selesai untuk menutup Setup Wizard."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Komputer anda harus di reboot untuk menyelesaikan proses membuang $(^NameDA). Reboot sekarang?"

  !define MUI_UNTEXT_ABORTWARNING "Adakan anda yakin ingin membatalkan proses buang $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END
