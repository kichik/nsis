#include "config.h"
#include <shlobj.h>

extern char ps_tmpbuf[NSIS_MAX_STRLEN*2];
char * NSISCALL GetNSISString(char *outbuf, int strtab);
#define GetNSISStringTT(strtab) GetNSISString(0, (strtab))
#define GetNSISStringNP(strtab) ((const char *)g_blocks[NB_STRINGS].offset+(strtab))
#define GetNSISTab(strtab) (strtab < 0 ? LANG_STR_TAB(strtab) : strtab)
void NSISCALL myRegGetStr(HKEY root, const char *sub, const char *name, char *out);
int NSISCALL myatoi(char *s);
void NSISCALL myitoa(char *s, int d);
char * NSISCALL mystrcpy(char *out, const char *in);
int NSISCALL mystrlen(const char *in);
char * NSISCALL mystrstr(char *a, char *b);
WIN32_FIND_DATA * NSISCALL file_exists(char *buf);
char * NSISCALL my_GetTempFileName(char *buf, const char *dir);
int NSISCALL my_PIDL2Path(char *out, LPITEMIDLIST idl);

//BOOL NSISCALL my_SetWindowText(HWND hWnd, const char *val);
#define my_SetWindowText SetWindowText
BOOL NSISCALL my_SetDialogItemText(HWND dlg, UINT idx, const char *val);
//#define my_SetDialogItemText SetDlgItemText
//int NSISCALL my_GetWindowText(HWND hWnd, char *val, int size);
#define my_GetWindowText GetWindowText
int NSISCALL my_GetDialogItemText(UINT idx, char *val);
//#define my_GetDialogItemText GetDlgItemText

#ifdef NSIS_CONFIG_LOG
extern char log_text[NSIS_MAX_STRLEN*4];
void NSISCALL log_write(int close);
void log_printf(char *format, ...);
#define log_printf2(x1,x2) log_printf(x1,x2);
#define log_printf3(x1,x2,x3) log_printf(x1,x2,x3);
#define log_printf4(x1,x2,x3,x4) log_printf(x1,x2,x3,x4);
#define log_printf5(x1,x2,x3,x4,x5) log_printf(x1,x2,x3,x4,x5);
#define log_printf6(x1,x2,x3,x4,x5,x6) log_printf(x1,x2,x3,x4,x5,x6);
#define log_printf7(x1,x2,x3,x4,x5,x6,x7) log_printf(x1,x2,x3,x4,x5,x6,x7);
#define log_printf8(x1,x2,x3,x4,x5,x6,x7,x8) log_printf(x1,x2,x3,x4,x5,x6,x7,x8);
extern int log_dolog;
extern char g_log_file[1024];
#else
#define log_printf(x1)
#define log_printf2(x1,x2)
#define log_printf3(x1,x2,x3)
#define log_printf4(x1,x2,x3,x4)
#define log_printf5(x1,x2,x3,x4,x5)
#define log_printf6(x1,x2,x3,x4,x5,x6)
#define log_printf7(x1,x2,x3,x4,x5,x6,x7)
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
char * NSISCALL findchar(char *str, char c);
void NSISCALL trimslashtoend(char *buf);
char * NSISCALL skip_root(char *path);
int NSISCALL is_valid_instpath(char *s);
char * NSISCALL validate_filename(char *fn);
void NSISCALL MoveFileOnReboot(LPCTSTR pszExisting, LPCTSTR pszNew);
void * NSISCALL mini_memcpy(void *out, const void *in, int len);

// Turn a pair of chars into a word
// Turn four chars into a dword
#ifdef __BIG_ENDIAN__ // Not very likely, but, still...
#define CHAR2_TO_WORD(a,b) (((WORD)(b))|((a)<<8))
#define CHAR4_TO_DWORD(a,b,c,d) (((DWORD)CHAR2_TO_WORD(c,d))|(CHAR2_TO_WORD(a,b)<<16))
#else
#define CHAR2_TO_WORD(a,b) (((WORD)(a))|((b)<<8))
#define CHAR4_TO_DWORD(a,b,c,d) (((DWORD)CHAR2_TO_WORD(a,b))|(CHAR2_TO_WORD(c,d)<<16))
#endif
