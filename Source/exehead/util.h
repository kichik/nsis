#include "config.h"
#include "lang.h"

void recursive_create_directory(char *directory);

extern char ps_tmpbuf[NSIS_MAX_STRLEN*2];
void process_string(char *out, const char *in);
void process_string_fromtab(char *out, int offs);
void process_string_from_lang(char *out, langid_t id);
int GetLangString(langid_t id);
int process_string_fromtab_toint(int offs);
void myRegGetStr(HKEY root, const char *sub, const char *name, char *out);
int myatoi(char *s);
void myitoa(char *s, int d);

#ifdef NSIS_CONFIG_LOG
extern char log_text[NSIS_MAX_STRLEN*4];
void log_write(int close);
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

HANDLE myCreateProcess(char *cmd, char *dir);
int my_MessageBox(const char *text, UINT type);

void doRMDir(char *buf, int recurse);

HANDLE myOpenFile(const char *fn, DWORD da, DWORD cd);
int CreateShortCut(HWND hwnd, LPCSTR pszShortcutFile, LPCSTR pszIconFile, int iconindex, LPCSTR pszExe, LPCSTR pszArg, LPCSTR workingdir, int showmode, int hotkey);
int validpathspec(char *ubuf);
void addtrailingslash(char *str);
char lastchar(const char *str);
void trimslashtoend(char *buf);
char *scanendslash(const char *str);
int is_valid_instpath(char *s);
BOOL MoveFileOnReboot(LPCTSTR pszExisting, LPCTSTR pszNew);
void *mini_memcpy(void *out, const void *in, int len);
