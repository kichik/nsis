extern char temp_directory[NSIS_MAX_STRLEN];

extern char g_usrvars[26][NSIS_MAX_STRLEN];
extern char *state_command_line;
extern char *state_install_directory;
extern char *state_output_directory;
extern char *state_exe_directory;
extern char *state_language;
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
extern char *state_plugins_dir;
#endif

extern char g_caption[NSIS_MAX_STRLEN*2];
extern HWND g_hwnd;
extern int g_filehdrsize;
extern HANDLE g_hInstance;
extern HWND insthwnd,insthwndbutton;
extern HICON g_hIcon;

extern int inst_flags;