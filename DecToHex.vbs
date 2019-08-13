' Marius Negrutiu (mailto:marius.negrutiu@protonmail.com) :: 2018/12/07

sHex = ""
iLen = 0

If WScript.Arguments.Count >= 1 Then
	sHex = Hex(WScript.Arguments(0))
End If

If WScript.Arguments.Count >= 2 Then
	iLen = CInt(WScript.Arguments(1))
End If

If Len(sHex) < iLen Then
	WScript.Echo String(iLen - Len(sHex), "0") & sHex
Else
	WScript.Echo sHex
End If