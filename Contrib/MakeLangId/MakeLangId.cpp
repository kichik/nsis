/*********
MakeLangID
**********

History
=======
20161207 - anders_k
* Rewritten from scratch to support languages from EnumSystemLocales
+ New icon ("Swerica" by anders_k, ZLib & CC BY-SA 4.0 dual license)

20130906 - anders_k
+ 64-bit support

20100326 - wizou & anders_k
+ Unicode support

20040818 - kichik
* Initial release

Reference
=========
https://msdn.microsoft.com/library/dd318693#Language Identifier Constants and Strings
https://wayback.archive.org/web/20021221200122/http://msdn.microsoft.com/library/en-us/intl/nls_8rse.asp#LOCALE_* (95/98/ME/NT4/2000/XP)

*/

#include "../../Source/Platform.h"
#include <commctrl.h>
#include "resource.h"

#if defined(_MSC_VER) && _MSC_VER-0 >= 1600 && _MSC_VER-0 <= 1800 // MSVC complains about math stuff we are not even using (bug #1159)
EXTERN_C int _fltused = 0;
EXTERN_C double _hypot(double x, double y) { return 0.0; }
#endif

#ifndef LOCALE_SNAME
#define LOCALE_SNAME 0x005C
#endif
#ifndef LOCALE_SENGLISHDISPLAYNAME
#define LOCALE_SENGLISHDISPLAYNAME 0x0072
#endif
#ifndef LOCALE_SNATIVEDISPLAYNAME
#define LOCALE_SNATIVEDISPLAYNAME 0x0073
#endif
#ifndef CB_SETCUEBANNER
#define CB_SETCUEBANNER (0x1700+3)
#endif

#if defined(_MSC_VER) && _MSC_VER >= 1200
EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#define HINST_THISCOMPONENT ( (HINSTANCE) &__ImageBase )
#define HINST_APPLICATION HINST_THISCOMPONENT
#else
#define HINST_APPLICATION ( (HINSTANCE) GetModuleHandle(NULL) )
#endif


static INT_PTR StrToIntptr(LPCTSTR s, bool ForceHex = false)
{
	UINT_PTR val = 0;
	int sign = 1, base = 10, numtop = '9';
	if (*s == TEXT('-')) s++, sign = -1;
	if (*s == TEXT('0'))
	{
		s++;
		if ((s[0] & ~0x20) == TEXT('X')) ++s, base = 16;
	}
	if (ForceHex) base = 16;
	for (;;)
	{
		int c = *s++;
		if (c >= _T('0') && c <= numtop) c -= TEXT('0');
		else if (base == 16 && (c & ~0x20) >= TEXT('A') && (c & ~0x20) <= TEXT('F')) c = (c & 7) + 9;
		else break;
		val *= base, val += c;
	}
	return ((INT_PTR)val) * sign;
}

typedef struct { WORD id; LPCSTR name; } INTLNG; // Storing the names as ASCII saves 4 KiB

#define IL(p, s, pn, sn) { MAKELANGID(p, s), #pn ":" #sn }
static const INTLNG g_IntLang[] = {
	IL(0x00, 0x00, NEUTRAL, NEUTRAL),
	IL(0x00, 0x01, NEUTRAL, DEFAULT),
	IL(0x00, 0x03, NEUTRAL, CUSTOM_DEFAULT), // Vista+? //archives.miloush.net/michkap/archive/2007/09/02/4701403.html
	IL(0x00, 0x04, NEUTRAL, CUSTOM_UNSPECIFIED), // Vista+?
	IL(0x00, 0x05, NEUTRAL, UI_CUSTOM_DEFAULT), // Vista+?
	IL(0x00, 0x08, NEUTRAL, DYNAMIC),
	IL(0x7f, 0x00, INVARIANT, NEUTRAL), // Invariant locale

	IL(0x36, 0x01, AFRIKAANS, AFRIKAANS_SOUTH_AFRICA),
	IL(0x1c, 0x01, ALBANIAN, ALBANIAN_ALBANIA),
	IL(0x01, 0x01, ARABIC, ARABIC_SAUDI_ARABIA),
	IL(0x01, 0x02, ARABIC, ARABIC_IRAQ),
	IL(0x01, 0x03, ARABIC, ARABIC_EGYPT),
	IL(0x01, 0x04, ARABIC, ARABIC_LIBYA),
	IL(0x01, 0x05, ARABIC, ARABIC_ALGERIA),
	IL(0x01, 0x06, ARABIC, ARABIC_MOROCCO),
	IL(0x01, 0x07, ARABIC, ARABIC_TUNISIA),
	IL(0x01, 0x08, ARABIC, ARABIC_OMAN),
	IL(0x01, 0x09, ARABIC, ARABIC_YEMEN),
	IL(0x01, 0x0a, ARABIC, ARABIC_SYRIA),
	IL(0x01, 0x0b, ARABIC, ARABIC_JORDAN),
	IL(0x01, 0x0c, ARABIC, ARABIC_LEBANON),
	IL(0x01, 0x0d, ARABIC, ARABIC_KUWAIT),
	IL(0x01, 0x0e, ARABIC, ARABIC_UAE),
	IL(0x01, 0x0f, ARABIC, ARABIC_BAHRAIN),
	IL(0x01, 0x10, ARABIC, ARABIC_QATAR),
	IL(0x2b, 0x01, ARMENIAN, ARMENIAN_ARMENIA),
	IL(0x4d, 0x01, ASSAMESE, ASSAMESE_INDIA),
	IL(0x2c, 0x01, AZERI, AZERI_LATIN),
	IL(0x2c, 0x02, AZERI, AZERI_CYRILLIC),
	IL(0x2d, 0x01, BASQUE, BASQUE_BASQUE),
	IL(0x23, 0x01, BELARUSIAN, BELARUSIAN_BELARUS),
	IL(0x45, 0x01, BENGALI, BENGALI_INDIA),
	IL(0x45, 0x02, BENGALI, BENGALI_BANGLADESH),
	IL(0x02, 0x01, BULGARIAN, BULGARIAN_BULGARIA),
	IL(0x03, 0x01, CATALAN, CATALAN_CATALAN),
	IL(0x5c, 0x01, CHEROKEE, CHEROKEE_CHEROKEE),
	IL(0x04, 0x01, CHINESE, CHINESE_TRADITIONAL),
	IL(0x04, 0x02, CHINESE, CHINESE_SIMPLIFIED),
	IL(0x04, 0x03, CHINESE, CHINESE_HONGKONG),
	IL(0x04, 0x04, CHINESE, CHINESE_SINGAPORE),
	IL(0x04, 0x05, CHINESE, CHINESE_MACAU),
	IL(0x83, 0x01, CORSICAN, CORSICAN_FRANCE),
	IL(0x1a, 0x01, CROATIAN, CROATIAN_CROATIA),
	IL(0x1a, 0x04, CROATIAN, CROATIAN_BOSNIA_HERZEGOVINA_LATIN),
	IL(0x05, 0x01, CZECH, CZECH_CZECH_REPUBLIC),
	IL(0x06, 0x01, DANISH, DANISH_DENMARK),
	IL(0x65, 0x01, DIVEHI, DIVEHI_MALDIVES),
	IL(0x13, 0x01, DUTCH, DUTCH),
	IL(0x13, 0x02, DUTCH, DUTCH_BELGIAN),
	IL(0x09, 0x01, ENGLISH, ENGLISH_US),
	IL(0x09, 0x02, ENGLISH, ENGLISH_UK),
	IL(0x09, 0x03, ENGLISH, ENGLISH_AUS),
	IL(0x09, 0x04, ENGLISH, ENGLISH_CAN),
	IL(0x09, 0x05, ENGLISH, ENGLISH_NZ),
	IL(0x09, 0x06, ENGLISH, ENGLISH_EIRE),
	IL(0x09, 0x07, ENGLISH, ENGLISH_SOUTH_AFRICA),
	IL(0x09, 0x08, ENGLISH, ENGLISH_JAMAICA),
	IL(0x09, 0x09, ENGLISH, ENGLISH_CARIBBEAN),
	IL(0x09, 0x0a, ENGLISH, ENGLISH_BELIZE),
	IL(0x09, 0x0b, ENGLISH, ENGLISH_TRINIDAD),
	IL(0x09, 0x0c, ENGLISH, ENGLISH_ZIMBABWE),
	IL(0x09, 0x0d, ENGLISH, ENGLISH_PHILIPPINES),
	IL(0x09, 0x10, ENGLISH, ENGLISH_INDIA),
	IL(0x09, 0x11, ENGLISH, ENGLISH_MALAYSIA),
	IL(0x09, 0x12, ENGLISH, ENGLISH_SINGAPORE),
	IL(0x25, 0x01, ESTONIAN, ESTONIAN_ESTONIA),
	IL(0x38, 0x01, FAEROESE, FAEROESE_FAROE_ISLANDS),
	IL(0x29, 0x01, FARSI, PERSIAN_IRAN), // LANG_FARSI AKA LANG_PERSIAN
	IL(0x64, 0x01, FILIPINO, FILIPINO_PHILIPPINES),
	IL(0x0b, 0x01, FINNISH, FINNISH_FINLAND),
	IL(0x0c, 0x01, FRENCH, FRENCH),
	IL(0x0c, 0x02, FRENCH, FRENCH_BELGIAN),
	IL(0x0c, 0x03, FRENCH, FRENCH_CANADIAN),
	IL(0x0c, 0x04, FRENCH, FRENCH_SWISS),
	IL(0x0c, 0x05, FRENCH, FRENCH_LUXEMBOURG),
	IL(0x0c, 0x06, FRENCH, FRENCH_MONACO),
	IL(0x56, 0x01, GALICIAN, GALICIAN_GALICIAN),
	IL(0x37, 0x01, GEORGIAN, GEORGIAN_GEORGIA),
	IL(0x07, 0x01, GERMAN, GERMAN),
	IL(0x07, 0x02, GERMAN, GERMAN_SWISS),
	IL(0x07, 0x03, GERMAN, GERMAN_AUSTRIAN),
	IL(0x07, 0x04, GERMAN, GERMAN_LUXEMBOURG),
	IL(0x07, 0x05, GERMAN, GERMAN_LIECHTENSTEIN),
	IL(0x08, 0x01, GREEK, GREEK_GREECE),
	IL(0x47, 0x01, GUJARATI, GUJARATI_INDIA),
	IL(0x75, 0x01, HAWAIIAN, HAWAIIAN_US),
	IL(0x0d, 0x01, HEBREW, HEBREW_ISRAEL),
	IL(0x39, 0x01, HINDI, HINDI_INDIA),
	IL(0x0e, 0x01, HUNGARIAN, HUNGARIAN_HUNGARY),
	IL(0x0f, 0x01, ICELANDIC, ICELANDIC_ICELAND),
	IL(0x21, 0x01, INDONESIAN, INDONESIAN_INDONESIA),
	IL(0x10, 0x01, ITALIAN, ITALIAN),
	IL(0x10, 0x02, ITALIAN, ITALIAN_SWISS),
	IL(0x11, 0x01, JAPANESE, JAPANESE_JAPAN),
	IL(0x60, 0x02, KASHMIRI, KASHMIRI_SASIA),
	IL(0x3f, 0x01, KAZAK, KAZAK_KAZAKHSTAN),
	IL(0x53, 0x01, KHMER, KHMER_CAMBODIA),
	IL(0x12, 0x01, KOREAN, KOREAN),
	IL(0x40, 0x01, KYRGYZ, KYRGYZ_KYRGYZSTAN),
	IL(0x54, 0x01, LAO, LAO_LAO),
	IL(0x26, 0x01, LATVIAN, LATVIAN_LATVIA),
	IL(0x27, 0x01, LITHUANIAN, LITHUANIAN),
	IL(0x2f, 0x01, MACEDONIAN, MACEDONIAN_MACEDONIA),
	IL(0x3e, 0x01, MALAY, MALAY_MALAYSIA),
	IL(0x3e, 0x02, MALAY, MALAY_BRUNEI_DARUSSALAM),
	IL(0x4c, 0x01, MALAYALAM, MALAYALAM_INDIA),
	IL(0x3a, 0x01, MALTESE, MALTESE_MALTA),
	//(0x58, 0x01, MANIPURI, ?),
	IL(0x4e, 0x01, MARATHI, MARATHI_INDIA),
	IL(0x50, 0x01, MONGOLIAN, MONGOLIAN_CYRILLIC_MONGOLIA),
	IL(0x50, 0x02, MONGOLIAN, MONGOLIAN_PRC),
	IL(0x61, 0x01, NEPALI, NEPALI_NEPAL),
	IL(0x61, 0x02, NEPALI, NEPALI_INDIA),
	IL(0x14, 0x01, NORWEGIAN, NORWEGIAN_BOKMAL),
	IL(0x14, 0x02, NORWEGIAN, NORWEGIAN_NYNORSK),
	IL(0x48, 0x01, ORIYA, ORIYA_INDIA),
	IL(0x15, 0x01, POLISH, POLISH_POLAND),
	IL(0x16, 0x01, PORTUGUESE, PORTUGUESE_BRAZILIAN),
	IL(0x16, 0x02, PORTUGUESE, PORTUGUESE),
	IL(0x18, 0x01, ROMANIAN, ROMANIAN_ROMANIA),
	IL(0x19, 0x01, RUSSIAN, RUSSIAN_RUSSIA),
	IL(0x4f, 0x01, SANSKRIT, SANSKRIT_INDIA),
	IL(0x91, 0x01, SCOTTISH_GAELIC, SCOTTISH_GAELIC),
	IL(0x1a, 0x01, SERBIAN, SERBIAN_CROATIA),
	IL(0x1a, 0x02, SERBIAN, SERBIAN_LATIN), // Serbia and Montenegro (former)
	IL(0x1a, 0x03, SERBIAN, SERBIAN_CYRILLIC), // Serbia and Montenegro (former)
	IL(0x1a, 0x06, SERBIAN, SERBIAN_BOSNIA_HERZEGOVINA_LATIN),
	IL(0x1a, 0x07, SERBIAN, SERBIAN_BOSNIA_HERZEGOVINA_CYRILLIC),
	IL(0x1b, 0x01, SLOVAK, SLOVAK_SLOVAKIA),
	IL(0x24, 0x01, SLOVENIAN, SLOVENIAN_SLOVENIA),
	IL(0x0a, 0x01, SPANISH, SPANISH),
	IL(0x0a, 0x02, SPANISH, SPANISH_MEXICAN),
	IL(0x0a, 0x03, SPANISH, SPANISH_MODERN),
	IL(0x0a, 0x04, SPANISH, SPANISH_GUATEMALA),
	IL(0x0a, 0x05, SPANISH, SPANISH_COSTA_RICA),
	IL(0x0a, 0x06, SPANISH, SPANISH_PANAMA),
	IL(0x0a, 0x07, SPANISH, SPANISH_DOMINICAN_REPUBLIC),
	IL(0x0a, 0x08, SPANISH, SPANISH_VENEZUELA),
	IL(0x0a, 0x09, SPANISH, SPANISH_COLOMBIA),
	IL(0x0a, 0x0a, SPANISH, SPANISH_PERU),
	IL(0x0a, 0x0b, SPANISH, SPANISH_ARGENTINA),
	IL(0x0a, 0x0c, SPANISH, SPANISH_ECUADOR),
	IL(0x0a, 0x0d, SPANISH, SPANISH_CHILE),
	IL(0x0a, 0x0e, SPANISH, SPANISH_URUGUAY),
	IL(0x0a, 0x0f, SPANISH, SPANISH_PARAGUAY),
	IL(0x0a, 0x10, SPANISH, SPANISH_BOLIVIA),
	IL(0x0a, 0x11, SPANISH, SPANISH_EL_SALVADOR),
	IL(0x0a, 0x12, SPANISH, SPANISH_HONDURAS),
	IL(0x0a, 0x13, SPANISH, SPANISH_NICARAGUA),
	IL(0x0a, 0x14, SPANISH, SPANISH_PUERTO_RICO),
	IL(0x0a, 0x15, SPANISH, SPANISH_US),
	IL(0x41, 0x01, SWAHILI, SWAHILI),
	IL(0x1d, 0x01, SWEDISH, SWEDISH),
	IL(0x1d, 0x02, SWEDISH, SWEDISH_FINLAND),
	IL(0x5a, 0x01, SYRIAC, SYRIAC_SYRIA),
	IL(0x1e, 0x01, THAI, THAI_THAILAND),
	IL(0x51, 0x01, TIBETAN, TIBETAN_PRC),
	IL(0x1f, 0x01, TURKISH, TURKISH_TURKEY),
	IL(0x22, 0x01, UKRAINIAN, UKRAINIAN_UKRAINE),
	IL(0x43, 0x01, UZBEK, UZBEK_LATIN),
	IL(0x43, 0x02, UZBEK, UZBEK_CYRILLIC),
	IL(0x2a, 0x01, VIETNAMESE, VIETNAMESE_VIETNAM),
	IL(0x52, 0x01, WELSH, WELSH_UNITED_KINGDOM)
};

HWND g_hList;

static INT AddLocale(HWND hCtl, LPCTSTR Name, UINT LangId)
{
	INT idx = (INT) SendMessage(hCtl, CB_ADDSTRING, 0, (LPARAM) Name);
	if (idx != CB_ERR) SendMessage(hCtl, CB_SETITEMDATA, idx, LangId);
	return idx;
}

static BOOL CALLBACK EnumSysLocalesProc(LPTSTR lpLocaleString)
{
	const UINT cchbuf1 = 80+2+80+1+2+85+1+!0, cchbuf2 = 85+!0;
	TCHAR buf1[cchbuf1], buf2[cchbuf2];
	UINT lid = (UINT) StrToIntptr(lpLocaleString, true), lcid = MAKELCID(lid, SORT_DEFAULT);

	UINT retval = GetLocaleInfo(lcid, LOCALE_SENGLISHDISPLAYNAME|LOCALE_NOUSEROVERRIDE, buf1, cchbuf1);
	if (!retval) // LOCALE_SENGLISHDISPLAYNAME is Win7+
	{
		GetLocaleInfo(lcid, LOCALE_SENGCOUNTRY|LOCALE_NOUSEROVERRIDE, buf2, cchbuf2);
		if ((retval = GetLocaleInfo(lcid, LOCALE_SENGLANGUAGE|LOCALE_NOUSEROVERRIDE, buf1, cchbuf1))) --retval;
		retval += wsprintf(buf1+retval, retval ? TEXT(" (%s)") : TEXT("?"), buf2);
	}
	AddLocale(g_hList, buf1, lid);
	return true;
}

static void InitSysLangList(HWND hCtl)
{
	g_hList = hCtl; // EnumSystemLocales is stupid and does not have a callback parameter
	SendMessage(hCtl, CB_SETCUEBANNER, 0, (LPARAM) L"Select a language...");
	EnumSystemLocales(EnumSysLocalesProc, LCID_SUPPORTED);
}

static void InitIntLangList(HWND hCtl)
{
	g_hList = hCtl;
	SendMessage(hCtl, CB_SETCUEBANNER, 0, (LPARAM) L"Select a language...");
	for (UINT i = 0; i < sizeof(g_IntLang)/sizeof(INTLNG); ++i)
	{
		#ifdef UNICODE
		WCHAR name[200];
		wsprintf(name, L"%S", g_IntLang[i].name);
		#else
		LPCTSTR name = g_IntLang[i].name;
		#endif
		AddLocale(hCtl, name, g_IntLang[i].id);
	}
}


static void OnLanguageChanged(HWND hDlg)
{
	const UINT cchbuf1 = 80+2+80+1+2+85+1+!0, cchbuf2 = 85+!0;
	TCHAR buf1[cchbuf1], buf2[cchbuf2];
	HWND hList = g_hList;

	INT idx = (INT) SendMessage(hList, CB_GETCURSEL, 0, 0);
	UINT lid = (UINT) SendMessage(hList, CB_GETITEMDATA, idx, 0), lcid = MAKELCID(lid, SORT_DEFAULT), retval;
	EnableWindow(GetDlgItem(hDlg, IDOK), idx != CB_ERR);
	if (idx == CB_ERR)
	{
		SetDlgItemText(hDlg, IDC_INFO, TEXT(""));
		SetDlgItemText(hDlg, IDC_LANGID, TEXT(""));
		SetDlgItemText(hDlg, IDC_CODEPAGE, TEXT(""));
		return ;
	}

	if ((retval = GetLocaleInfo(lcid, LOCALE_SNATIVEDISPLAYNAME|LOCALE_NOUSEROVERRIDE, buf1, cchbuf1))) --retval;
	if (!retval) // LOCALE_SNATIVEDISPLAYNAME is Win7+
	{
		GetLocaleInfo(lcid, LOCALE_SNATIVECTRYNAME|LOCALE_NOUSEROVERRIDE, buf2, cchbuf2);
		if ((retval = GetLocaleInfo(lcid, LOCALE_SNATIVELANGNAME|LOCALE_NOUSEROVERRIDE, buf1, cchbuf1))) --retval;
		retval += wsprintf(buf1+retval, retval ? TEXT(" (%s)") : TEXT("?"), buf2);
	}

	if (retval && GetLocaleInfo(lcid, LOCALE_SNAME|LOCALE_NOUSEROVERRIDE, buf2, cchbuf2) && *buf2)
	{
		wsprintf(buf1+retval, TEXT(" [%s]"), buf2);
	}
	SetDlgItemText(hDlg, IDC_INFO, buf1);

	wsprintf(buf1, TEXT("%u (0x%X)"), lid, lid);
	SetDlgItemText(hDlg, IDC_LANGID, buf1);
	wsprintf(buf1, TEXT("%u"), lid);
	SetDlgItemText(hDlg, IDC_COPYHELPER, buf1);

	retval = GetLocaleInfo(lcid, LOCALE_IDEFAULTANSICODEPAGE|LOCALE_NOUSEROVERRIDE, buf2, cchbuf2);
	LPCTSTR cpstr = retval ? lstrcmp(buf2, TEXT("0")) ? buf2 : TEXT("1200 (Unicode-only)") : TEXT("?");
	SetDlgItemText(hDlg, IDC_CODEPAGE, cpstr);
}

INT_PTR CALLBACK DialogProc(HWND hDlg, UINT Msg, WPARAM wp, LPARAM lp)
{
	HWND hCtl, hCtl2;
	switch(Msg)
	{
	case WM_COMMAND:
		switch(wp)
		{
		case MAKELONG(IDOK, BN_CLICKED):
			// Using a secret edit box to copy to the clipboard
			hCtl = GetDlgItem(hDlg, IDC_COPYHELPER);
			SendMessage(hCtl, EM_SETSEL, 0, -1);
			SendMessage(hCtl, WM_COPY, 0, 0);
			break;

		case MAKELONG(IDCANCEL, BN_CLICKED):
			return EndDialog(hDlg, 0);

		case MAKELONG(IDC_SOURCE, CBN_SELCHANGE):
			hCtl = GetDlgItem(hDlg, IDC_SYSLANGLIST), hCtl2 = GetDlgItem(hDlg, IDC_INTLANGLIST);
			if (g_hList == hCtl) { HWND h; h = hCtl, hCtl = hCtl2, hCtl2 = h; }
			ShowWindow(g_hList = hCtl, SW_SHOW);
			ShowWindow(hCtl2, SW_HIDE);
			// fallthrough

		case MAKELONG(IDC_SYSLANGLIST, CBN_SELCHANGE):
		case MAKELONG(IDC_INTLANGLIST, CBN_SELCHANGE):
			OnLanguageChanged(hDlg);
			break;
		}
		break;

	case WM_INITDIALOG:
		{
			SendMessage(hDlg, WM_SETICON, ICON_BIG, lp);
			InitSysLangList(GetDlgItem(hDlg, IDC_SYSLANGLIST));
			InitIntLangList(GetDlgItem(hDlg, IDC_INTLANGLIST));
			hCtl = GetDlgItem(hDlg, IDC_SOURCE);
			SendMessage(hCtl, CB_ADDSTRING, 0, (LPARAM) TEXT("System"));
			SendMessage(hCtl, CB_ADDSTRING, 0, (LPARAM) TEXT("Internal"));
			SendMessage(hCtl, CB_SETCURSEL, 0, 0); // Select "System"...
			SendMessage(hDlg, WM_COMMAND, MAKELONG(IDC_SOURCE, CBN_SELCHANGE), (LPARAM) hCtl); // ...and notify
			PostMessage(hDlg, WM_NEXTDLGCTL, 0, FALSE); // Switches focus to the 2nd combobox
		}
		break;
	}
	return FALSE;
}

NSIS_ENTRYPOINT_GUINOCRT
EXTERN_C void NSISWinMainNOCRT()
{
	InitCommonControls();
	HINSTANCE hInst = HINST_APPLICATION;
	HANDLE hIco = LoadImage(hInst, MAKEINTRESOURCE(IDI_ICON), IMAGE_ICON, 0, 0, LR_DEFAULTSIZE|LR_SHARED);
	INT_PTR retval = DialogBoxParam(hInst, MAKEINTRESOURCE(IDD_DIALOG), 0, DialogProc, (LPARAM) hIco);
	ExitProcess((UINT) retval);
}
