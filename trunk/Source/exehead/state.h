#ifdef NSIS_SUPPORT_NAMED_USERVARS
  extern NSIS_STRING g_usrvars[TOTAL_COMPATIBLE_STATIC_VARS_COUNT];
  #define state_command_line        g_usrvars[20]
  #define state_install_directory   g_usrvars[21]
  #define state_output_directory    g_usrvars[22]
  #define state_exe_directory       g_usrvars[23]
  #define state_language            g_usrvars[24] 
  #ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    #define state_plugins_dir       g_usrvars[25]
  #endif
  #define state_temp_dir            g_usrvars[32]
#else
  extern char temp_directory[NSIS_MAX_STRLEN];
  extern char g_usrvars[USER_VARS_COUNT][NSIS_MAX_STRLEN];
  extern char *state_command_line;
  extern char *state_install_directory;
  extern char *state_output_directory;
  extern char *state_exe_directory;
  extern char *state_language;
  #ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    extern char *state_plugins_dir;
  #endif
  #define state_temp_dir temp_directory
#endif

extern char g_caption[NSIS_MAX_STRLEN*2];
extern HWND g_hwnd;
extern int g_filehdrsize;
extern HANDLE g_hInstance;
extern HWND insthwnd,insthwndbutton;
extern HICON g_hIcon;

extern int inst_flags;