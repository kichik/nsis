extern NSIS_STRING g_usrvars[1];

#define state_command_line        g_usrvars[20]
#define state_install_directory   g_usrvars[21]
#define state_output_directory    g_usrvars[22]
#define state_exe_directory       g_usrvars[23]
#define state_language            g_usrvars[24] 
#define state_temp_dir            g_usrvars[25]
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
#  define state_plugins_dir       g_usrvars[26]
#endif
#define state_click_next          g_usrvars[28]

extern char g_caption[NSIS_MAX_STRLEN*2];
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
extern HWND g_hwnd;
extern HANDLE g_hInstance;
extern HWND insthwnd,insthwndbutton;
#else
#define g_hwnd 0
#define g_hInstance 0
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
