extern char g_usrvars[25][NSIS_MAX_STRLEN];
#define state_command_line (g_usrvars[20])
#define state_install_directory (g_usrvars[21])
#define state_output_directory (g_usrvars[22])
#define state_exe_directory (g_usrvars[23])
#define state_language (g_usrvars[24])

extern char g_caption[NSIS_MAX_STRLEN*2];
extern HWND g_hwnd;
extern int g_filehdrsize;
extern HANDLE g_hInstance;
extern HWND insthwnd,insthwndbutton;
