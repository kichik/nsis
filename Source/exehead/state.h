extern char g_usrvars[24][NSIS_MAX_STRLEN];
/*#define state_command_line (g_usrvars[20])
#define state_install_directory (g_usrvars[21])
#define state_output_directory (g_usrvars[22])
#define state_exe_directory (g_usrvars[23])*/
// changed by Amir Szekely 28th August 2002
// smaller exehead
extern char *state_command_line;
extern char *state_install_directory;
extern char *state_output_directory;
extern char *state_exe_directory;

extern char g_caption[NSIS_MAX_STRLEN*2];
extern HWND g_hwnd;
extern int g_filehdrsize;
extern HANDLE g_hInstance;
extern HWND insthwnd,insthwndbutton;
