;Default NSIS Config File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This file is treated as if it is in the directory
;that you are building from. i.e. the command:
;
;   File whatever.dat
;
;would need whatever.dat to be in your build directory.


;Change the default icon or bitmaps:
;
;Icon "${NSISDIR}\Contrib\Icons\setup.ico"
;CheckBitmap "${NSISDIR}\Contrib\Icons\checks4.bmp"

;Define Symbols:
;
;!define poo

;Use one of these if you have upx or petite installed.
;Note that your exe packer should not pack the first icon
;and paths should ideally be absolute (since you could be 
;building your installer anywhere).
;
;!packhdr tmp.dat "C:\program files\upx\upx -9 -q tmp.dat"
;!packhdr tmp.dat "C:\program files\petite\petite -9 -b0 -r** -p0 -y tmp.dat"
