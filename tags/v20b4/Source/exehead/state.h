#ifdef NSIS_SUPPORT_NAMED_USERVARS
  extern NSIS_STRING g_usrvars[TOTAL_COMPATIBLE_STATIC_VARS_COUNT];
#else
  extern char g_usrvars[USER_VARS_COUNT][NSIS_MAX_STRLEN];
#endif

#define state_command_line        g_usrvars[20]
#define state_install_directory   g_usrvars[21]
#define state_output_directory    g_usrvars[22]
#define state_exe_directory       g_usrvars[23]
#define state_language            g_usrvars[24] 
#define state_temp_dir            g_usrvars[25]
#define state_click_next          g_usrvars[26]
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  #define state_plugins_dir       g_usrvars[27]
#endif

extern char g_caption[NSIS_MAX_STRLEN*2];
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
extern HWND g_hwnd;
extern HANDLE g_hInstance;
extern HWND insthwnd,insthwndbutton;
extern HICON g_hIcon;
#else
#define g_hwnd 0
#define g_hInstance 0
#define g_hIcon 0
#endif//NSIS_CONFIG_VISIBLE_SUPPORT