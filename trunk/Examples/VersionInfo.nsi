; VersionInfo.nsi
;
; This script shows you how to add version information to an installer.
; Windows shows this information on the Version tab of the File properties.

;--------------------------------

Name "Version Info"

OutFile "VersionInfo.exe"

;--------------------------------
;Version Information

  VISetVersionLanguage 2057 1200 ; English UK
  VIAddTranslation 2057 1200     ; English UK
  VIProductVersion "1.2.3.4"
  VIAddVersionKey "ProductName" "Test Application"
  VIAddVersionKey "Comments" "A test comment"
  VIAddVersionKey "CompanyName" "Fake company"
  VIAddVersionKey "LegalTrademarks" "Test Application is a trademark of Fake company"
  VIAddVersionKey "LegalCopyright" "© Fake company"
  VIAddVersionKey "FileDescription" "Test Application"
  VIAddVersionKey "FileVersion" "1.2.3"

;--------------------------------

Section ""

SectionEnd