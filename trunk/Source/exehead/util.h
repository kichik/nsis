#include "config.h"

extern char ps_tmpbuf[NSIS_MAX_STRLEN*2];
#ifdef NSIS_SUPPORT_LANG_IN_STRINGS
char * NSISCALL process_string(const char *in, int iStartIdx);
#else
char * NSISCALL process_string(const char *in);
#endif
char * NSISCALL process_string_fromtab(char *out, int offs);
void NSISCALL myRegGetStr(HKEY root, const char *sub, const char *name, char *out);
int NSISCALL myatoi(char *s);
void NSISCALL myitoa(char *s, int d);
char * NSISCALL mystrcpy(char *out, const char *in);
int NSISCALL mystrlen(const char *in);
char * NSISCALL mystrstr(char *a, char *b);
WIN32_FIND_DATA * NSISCALL file_exists(char *buf);

//BOOL NSISCALL my_SetWindowText(HWND hWnd, const char *val);
#define my_SetWindowText SetWindowText
BOOL NSISCALL my_SetDialogItemText(HWND dlg, UINT idx, const char *val);
//int NSISCALL my_GetWindowText(HWND hWnd, char *val, int size);
#define my_GetWindowText GetWindowText
//int NSISCALL my_GetDialogItemText(HWND dlg, UINT idx, char *val, int size);
#define my_GetDialogItemText GetDlgItemText

#ifdef NSIS_CONFIG_LOG
extern char log_text[NSIS_MAX_STRLEN*4];
void NSISCALL log_write(int close);
#define log_printf(x1) wsprintf(log_text,x1); log_write(0)
#define log_printf2(x1,x2) wsprintf(log_text,x1,x2); log_write(0)
#define log_printf3(x1,x2,x3) wsprintf(log_text,x1,x2,x3); log_write(0)
#define log_printf4(x1,x2,x3,x4) wsprintf(log_text,x1,x2,x3,x4); log_write(0)
#define log_printf5(x1,x2,x3,x4,x5) wsprintf(log_text,x1,x2,x3,x4,x5); log_write(0)
#define log_printf6(x1,x2,x3,x4,x5,x6) wsprintf(log_text,x1,x2,x3,x4,x5,x6); log_write(0)
#define log_printf8(x1,x2,x3,x4,x5,x6,x7,x8) wsprintf(log_text,x1,x2,x3,x4,x5,x6,x7,x8); log_write(0)
extern int log_dolog;
extern char g_log_file[1024];
#else
#define log_printf(x1)
#define log_printf2(x1,x2)
#define log_printf3(x1,x2,x3)
#define log_printf4(x1,x2,x3,x4)
#define log_printf5(x1,x2,x3,x4,x5)
#define log_printf6(x1,x2,x3,x4,x5,x6)
#define log_printf8(x1,x2,x3,x4,x5,x6,x7,x8)
#endif

HANDLE NSISCALL myCreateProcess(char *cmd, char *dir);
int NSISCALL my_MessageBox(const char *text, UINT type);
void * NSISCALL my_GlobalAlloc(DWORD dwBytes);

void NSISCALL doRMDir(char *buf, int recurse);

HANDLE NSISCALL myOpenFile(const char *fn, DWORD da, DWORD cd);
int NSISCALL validpathspec(char *ubuf);
char * NSISCALL addtrailingslash(char *str);
//char NSISCALL lastchar(const char *str);
#define lastchar(str) *CharPrev(str,str+mystrlen(str))
void NSISCALL trimslashtoend(char *buf);
char * NSISCALL scanendslash(const char *str);
int NSISCALL is_valid_instpath(char *s);
char * NSISCALL validate_filename(char *fn);
BOOL NSISCALL MoveFileOnReboot(LPCTSTR pszExisting, LPCTSTR pszNew);
void * NSISCALL mini_memcpy(void *out, const void *in, int len);

// Turn a pair of chars into a word
// Turn four chars into a dword
#ifdef __BIG_ENDIAN__ // Not very likely, but, still...
#define CHAR2_TO_WORD(a,b) (((WORD)(b))|((a)<<8))
#define CHAR4_TO_DWORD(a,b,c,d)	(((DWORD)CHAR2_TO_WORD(c,d))|(CHAR2_TO_WORD(a,b)<<16))
#else
#define CHAR2_TO_WORD(a,b) (((WORD)(a))|((b)<<8))
#define CHAR4_TO_DWORD(a,b,c,d)	(((DWORD)CHAR2_TO_WORD(a,b))|(CHAR2_TO_WORD(c,d)<<16))
#endif
