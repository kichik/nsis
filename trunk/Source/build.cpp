#include "Platform.h"
#include <stdio.h>
#include "exehead/config.h"
#include "exehead/fileform.h"

#include "exedata.h"

#include "build.h"
#include "util.h"

#include "exehead/resource.h"
#include "ResourceEditor.h"
#include "DialogTemplate.h"
#include "ResourceVersionInfo.h"

#ifndef _WIN32
#  include <locale.h>
#  include <unistd.h>
#  include <limits.h>
#  include <stdlib.h>
#  include <stdarg.h>
#endif

#include <cassert> // for assert

#define RET_UNLESS_OK( function_rc ) do { \
  int rc = (function_rc); \
  if ( rc != PS_OK) \
    return rc; \
} while (false)

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
#ifdef _WIN32
DWORD WINAPI lzmaCompressThread(LPVOID lpParameter)
#else
void *lzmaCompressThread(void *lpParameter)
#endif
{
  CLZMA *Compressor = (CLZMA *) lpParameter;
  if (!Compressor)
    return 0;

  Compressor->CompressReal();
  return 0;
}
#endif

namespace { // begin anonymous namespace

bool isSimpleChar(char ch)
{
  return (ch == '.' ) || (ch == '_' ) || (ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}

} // end of anonymous namespace

void CEXEBuild::define(const char *p, const char *v)
{
  definedlist.add(p,v);
}

CEXEBuild::~CEXEBuild()
{
  free(header_data_new);
  free(m_unicon_data);
  
  int nlt = lang_tables.getlen() / sizeof(LanguageTable);
  LanguageTable *nla = (LanguageTable*)lang_tables.get();

  for (int i = 0; i < nlt; i++) {
    DeleteLangTable(nla+i);
  }
}

CEXEBuild::CEXEBuild()
{
  linecnt = 0;
  fp = 0;
  curfilename = 0;

  display_info=1;
  display_script=1;
  display_errors=1;
  display_warnings=1;

  cur_ifblock=NULL;
  last_line_had_slash=0;
  inside_comment=false;
  multiple_entries_instruction=0;

  build_include_depth=0;

  has_called_write_output=false;

  ns_func.add("",0); // make sure offset 0 is special on these (i.e. never used by a label)
  ns_label.add("",0);

  header_data_new=(unsigned char*)malloc(zlib_exeheader_size);
  exeheader_size_new=zlib_exeheader_size;
  exeheader_size=zlib_exeheader_size;

  if (!header_data_new)
  {
    ERROR_MSG("Internal compiler error #12345: malloc(%d) failed\n",exeheader_size_new);
    extern void quit(); quit();
  }

  // Changed by Amir Szekely 31st July 2002
  memcpy(header_data_new,zlib_header_data,zlib_exeheader_size);

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  // Changed by Amir Szekely 11th July 2002
  // No need to check for uninstaller icon if uninstall support is disabled.
  if (unicondata_size != icondata_size)
  {
    ERROR_MSG("Internal compiler error #12345: installer,uninstaller icon size mismatch (%d,%d)\n",icondata_size,unicondata_size);
    extern void quit(); quit();
  }
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT

  extern const char *NSIS_VERSION;
  definedlist.add("NSIS_VERSION", NSIS_VERSION);

#define intdef2str_(x) #x
#define intdef2str(x) intdef2str_(x)
  definedlist.add("NSIS_MAX_INST_TYPES", intdef2str(NSIS_MAX_INST_TYPES));
  definedlist.add("NSIS_MAX_STRLEN", intdef2str(NSIS_MAX_STRLEN));
  definedlist.add("NSIS_DEFAULT_LANG", intdef2str(NSIS_DEFAULT_LANG));
  definedlist.add("NSIS_COMPRESS_BZIP2_LEVEL", intdef2str(NSIS_COMPRESS_BZIP2_LEVEL));
#undef intdef2str
#undef intdef2str_
#ifdef NSIS_BZIP2_COMPRESS_WHOLE
  definedlist.add("NSIS_BZIP2_COMPRESS_WHOLE");
#endif
#ifdef NSIS_COMPRESS_BZIP2_SMALLMODE
  definedlist.add("NSIS_COMPRESS_BZIP2_SMALLMODE");
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
  definedlist.add("NSIS_CONFIG_COMPONENTPAGE");
#endif
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  definedlist.add("NSIS_CONFIG_COMPRESSION_SUPPORT");
#endif
#ifdef NSIS_CONFIG_CRC_ANAL
  definedlist.add("NSIS_CONFIG_CRC_ANAL");
#endif
#ifdef NSIS_CONFIG_CRC_SUPPORT
  definedlist.add("NSIS_CONFIG_CRC_SUPPORT");
#endif
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
  definedlist.add("NSIS_CONFIG_ENHANCEDUI_SUPPORT");
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
  definedlist.add("NSIS_CONFIG_LICENSEPAGE");
#endif
#ifdef NSIS_CONFIG_LOG
  definedlist.add("NSIS_CONFIG_LOG");
#endif
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  definedlist.add("NSIS_CONFIG_PLUGIN_SUPPORT");
#endif
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  definedlist.add("NSIS_CONFIG_SILENT_SUPPORT");
#endif
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  definedlist.add("NSIS_CONFIG_UNINSTALL_SUPPORT");
#endif
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  definedlist.add("NSIS_CONFIG_VISIBLE_SUPPORT");
#endif
#ifdef NSIS_SUPPORT_ACTIVEXREG
  definedlist.add("NSIS_SUPPORT_ACTIVEXREG");
#endif
#ifdef NSIS_SUPPORT_BGBG
  definedlist.add("NSIS_SUPPORT_BGBG");
#endif
#ifdef NSIS_SUPPORT_CODECALLBACKS
  definedlist.add("NSIS_SUPPORT_CODECALLBACKS");
#endif
#ifdef NSIS_SUPPORT_COPYFILES
  definedlist.add("NSIS_SUPPORT_COPYFILES");
#endif
#ifdef NSIS_SUPPORT_CREATESHORTCUT
  definedlist.add("NSIS_SUPPORT_CREATESHORTCUT");
#endif
#ifdef NSIS_SUPPORT_DELETE
  definedlist.add("NSIS_SUPPORT_DELETE");
#endif
#ifdef NSIS_SUPPORT_ENVIRONMENT
  definedlist.add("NSIS_SUPPORT_ENVIRONMENT");
#endif
#ifdef NSIS_SUPPORT_EXECUTE
  definedlist.add("NSIS_SUPPORT_EXECUTE");
#endif
#ifdef NSIS_SUPPORT_FILE
  definedlist.add("NSIS_SUPPORT_FILE");
#endif
#ifdef NSIS_SUPPORT_FILEFUNCTIONS
  definedlist.add("NSIS_SUPPORT_FILEFUNCTIONS");
#endif
#ifdef NSIS_SUPPORT_FINDFIRST
  definedlist.add("NSIS_SUPPORT_FINDFIRST");
#endif
#ifdef NSIS_SUPPORT_FNUTIL
  definedlist.add("NSIS_SUPPORT_FNUTIL");
#endif
#ifdef NSIS_SUPPORT_GETDLLVERSION
  definedlist.add("NSIS_SUPPORT_GETDLLVERSION");
#endif
#ifdef NSIS_SUPPORT_GETFILETIME
  definedlist.add("NSIS_SUPPORT_GETFILETIME");
#endif
#ifdef NSIS_SUPPORT_HWNDS
  definedlist.add("NSIS_SUPPORT_HWNDS");
#endif
#ifdef NSIS_SUPPORT_INIFILES
  definedlist.add("NSIS_SUPPORT_INIFILES");
#endif
#ifdef NSIS_SUPPORT_INTOPTS
  definedlist.add("NSIS_SUPPORT_INTOPTS");
#endif
#ifdef NSIS_SUPPORT_MESSAGEBOX
  definedlist.add("NSIS_SUPPORT_MESSAGEBOX");
#endif
#ifdef NSIS_SUPPORT_MOVEONREBOOT
  definedlist.add("NSIS_SUPPORT_MOVEONREBOOT");
#endif
#ifdef NSIS_SUPPORT_REBOOT
  definedlist.add("NSIS_SUPPORT_REBOOT");
#endif
#ifdef NSIS_SUPPORT_REGISTRYFUNCTIONS
  definedlist.add("NSIS_SUPPORT_REGISTRYFUNCTIONS");
#endif
#ifdef NSIS_SUPPORT_RENAME
  definedlist.add("NSIS_SUPPORT_RENAME");
#endif
#ifdef NSIS_SUPPORT_RMDIR
  definedlist.add("NSIS_SUPPORT_RMDIR");
#endif
#ifdef NSIS_SUPPORT_SHELLEXECUTE
  definedlist.add("NSIS_SUPPORT_SHELLEXECUTE");
#endif
#ifdef NSIS_SUPPORT_STACK
  definedlist.add("NSIS_SUPPORT_STACK");
#endif
#ifdef NSIS_SUPPORT_STROPTS
  definedlist.add("NSIS_SUPPORT_STROPTS");
#endif
#ifdef NSIS_ZLIB_COMPRESS_WHOLE
  definedlist.add("NSIS_ZLIB_COMPRESS_WHOLE");
#endif
#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  definedlist.add("NSIS_SUPPORT_STANDARD_PREDEFINES");
#endif

// no more optional
definedlist.add("NSIS_SUPPORT_NAMED_USERVARS");

#ifdef NSIS_SUPPORT_VERSION_INFO
  definedlist.add("NSIS_SUPPORT_VERSION_INFO");
#endif

// no more optional
definedlist.add("NSIS_SUPPORT_LANG_IN_STRINGS");

#ifdef NSIS_FIX_DEFINES_IN_STRINGS
  definedlist.add("NSIS_FIX_DEFINES_IN_STRINGS");
#endif

  db_opt_save=db_comp_save=db_full_size=db_opt_save_u=db_comp_save_u=db_full_size_u=0;

  // Added by Amir Szekely 31st July 2002
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  compressor = &zlib_compressor;
#endif
  build_compressor_set = false;
  build_compressor_final = false;
#ifdef NSIS_ZLIB_COMPRESS_WHOLE
  build_compress_whole = true;
#else
  build_compress_whole = false;
#endif
  build_compress=1;
  build_compress_level=9;
  build_compress_dict_size=1<<23;

  cur_entries=&build_entries;
  cur_instruction_entry_map=&build_instruction_entry_map;
  cur_datablock=&build_datablock;
  cur_datablock_cache=&build_datablock_cache;
  cur_functions=&build_functions;
  cur_labels=&build_labels;
  cur_sections=&build_sections;
  cur_header=&build_header;
  cur_strlist=&build_strlist;
  cur_langtables=&build_langtables;
  cur_ctlcolors=&build_ctlcolors;
  cur_pages=&build_pages;
  cur_page=0;
  cur_page_type=-1;

  build_filebuflen=32<<20; // 32mb

  subsection_open_cnt=0;
  build_cursection_isfunc=0;
  build_cursection=NULL;
  // init public data.
  build_packname[0]=build_packcmd[0]=build_output_filename[0]=0;

  // Added by ramon 23 May 2003
  build_allowskipfiles=1;

  // Added by ramon 6 jun 2003
#ifdef NSIS_SUPPORT_VERSION_INFO
  version_product_v[0]=0;
#endif

  build_overwrite=build_last_overwrite=0;
  build_crcchk=1;
  build_datesave=1;
  build_optimize_datablock=1;

  memset(&build_header,-1,sizeof(build_header));

  build_header.install_reg_rootkey=0;
  build_header.flags=CH_FLAGS_NO_ROOT_DIR;
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  build_header.lb_bg=RGB(0,0,0);
  build_header.lb_fg=RGB(0,255,0);
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
  build_header.license_bg=-COLOR_BTNFACE;
#endif
  build_header.install_directory_ptr=0;
  build_header.install_directory_auto_append=0;
  build_header.install_reg_key_ptr=0;
  build_header.install_reg_value_ptr=0;
#ifdef NSIS_CONFIG_COMPONENTPAGE
  memset(build_header.install_types,0,sizeof(build_header.install_types));
#endif
  memset(&build_header.blocks,0,sizeof(build_header.blocks));

  uninstall_mode=0;
  uninstall_size_full=0;
  uninstall_size=-1;

  memset(&build_uninst,-1,sizeof(build_uninst));

  build_header.install_reg_rootkey=0;
  build_uninst.flags=0;
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  build_uninst.lb_bg=RGB(0,0,0);
  build_uninst.lb_fg=RGB(0,255,0);
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
  build_uninst.license_bg=-COLOR_BTNFACE;
#endif
  build_uninst.install_directory_ptr=0;
  build_uninst.install_directory_auto_append=0;
  build_uninst.install_reg_key_ptr=0;
  build_uninst.install_reg_value_ptr=0;
#ifdef NSIS_CONFIG_COMPONENTPAGE
  memset(build_uninst.install_types,0,sizeof(build_uninst.install_types));
#endif
  memset(&build_uninst.blocks,0,sizeof(build_uninst.blocks));

  uninstaller_writes_used=0;

  build_strlist.add("",0);
  ubuild_strlist.add("",0);

  build_langstring_num=0;
  ubuild_langstring_num=0;

  build_font[0]=0;
  build_font_size=0;

  m_unicon_data=(unsigned char *)malloc(unicondata_size+3*sizeof(DWORD));
  memcpy(m_unicon_data+2*sizeof(DWORD),unicon_data+22,unicondata_size);
  *(DWORD*)(m_unicon_data) = unicondata_size;
  *(DWORD*)(m_unicon_data + sizeof(DWORD)) = 0;
  *(DWORD*)(m_unicon_data + 2*sizeof(DWORD) + unicondata_size) = 0;
  unicondata_size += 3*sizeof(DWORD);

  branding_image_found=false;

  no_space_texts=false;

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  build_plugin_unload=0;
  plugins_processed=0;
#endif

  last_used_lang=NSIS_DEFAULT_LANG;

  res_editor=0;

  enable_last_page_cancel=0;
  uenable_last_page_cancel=0;

  license_res_id=IDD_LICENSE;

  disable_window_icon=0;

#ifdef _WIN32
  notify_hwnd=0;
#endif

#ifdef NSIS_SUPPORT_BGBG
  bg_default_font.lfHeight=40;
  bg_default_font.lfWidth=0;
  bg_default_font.lfEscapement=0;
  bg_default_font.lfOrientation=0;
  bg_default_font.lfWeight=FW_BOLD;
  bg_default_font.lfItalic=TRUE;
  bg_default_font.lfUnderline=FALSE;
  bg_default_font.lfStrikeOut=FALSE;
  bg_default_font.lfCharSet=DEFAULT_CHARSET;
  bg_default_font.lfOutPrecision=OUT_DEFAULT_PRECIS;
  bg_default_font.lfClipPrecision=CLIP_DEFAULT_PRECIS;
  bg_default_font.lfQuality=DEFAULT_QUALITY;
  bg_default_font.lfPitchAndFamily=DEFAULT_PITCH;
  strncpy(bg_default_font.lfFaceName,"Times New Roman",LF_FACESIZE);
  memcpy(&bg_font,&bg_default_font,sizeof(LOGFONT));
#endif

  defcodepage_set=false;
  uDefCodePage=CP_ACP;

  InitLangTables();

  // Register static user variables $0, $1 and so on
  // with ONE of reference count, to avoid warning on this vars
  char Aux[3];
  int i;
  for (i = 0; i < 10; i++)    // 0 - 9
  {
    sprintf(Aux, "%d", i);    
    m_UserVarNames.add(Aux,1);
  }
  for (i = 0; i < 10; i++)        // 10 - 19
  {
    sprintf(Aux, "R%d", i);    
    m_UserVarNames.add(Aux,1);
  }
  m_UserVarNames.add("CMDLINE",1);       // 20 everything before here doesn't have trailing slash removal
  m_UserVarNames.add("INSTDIR",1);       // 21
  m_UserVarNames.add("OUTDIR",1);        // 22
  m_UserVarNames.add("EXEDIR",1);        // 23
  m_UserVarNames.add("LANGUAGE",1);      // 24
  m_UserVarNames.add("TEMP",-1);         // 25
  m_UserVarNames.add("PLUGINSDIR",-1);   // 26
  m_UserVarNames.add("HWNDPARENT",-1);   // 27
  m_UserVarNames.add("_CLICK",-1);       // 28

  m_iBaseVarsNum = m_UserVarNames.getnum();

  m_ShellConstants.add("WINDIR",CSIDL_WINDOWS,CSIDL_WINDOWS);
  m_ShellConstants.add("SYSDIR",CSIDL_SYSTEM,CSIDL_SYSTEM);
  m_ShellConstants.add("PROGRAMFILES",CSIDL_PROGRAM_FILES, CSIDL_PROGRAM_FILES);
  m_ShellConstants.add("SMPROGRAMS",CSIDL_PROGRAMS, CSIDL_COMMON_PROGRAMS);
  m_ShellConstants.add("SMSTARTUP",CSIDL_STARTUP, CSIDL_COMMON_STARTUP);
  m_ShellConstants.add("DESKTOP",CSIDL_DESKTOPDIRECTORY, CSIDL_COMMON_DESKTOPDIRECTORY);
  m_ShellConstants.add("STARTMENU",CSIDL_STARTMENU, CSIDL_COMMON_STARTMENU);
  m_ShellConstants.add("QUICKLAUNCH", CSIDL_APPDATA, CSIDL_PRINTERS);
  m_ShellConstants.add("COMMONFILES",CSIDL_PROGRAM_FILES_COMMON, CSIDL_PROGRAM_FILES_COMMON);
  m_ShellConstants.add("DOCUMENTS",CSIDL_PERSONAL, CSIDL_COMMON_DOCUMENTS);
  m_ShellConstants.add("SENDTO",CSIDL_SENDTO, CSIDL_SENDTO);
  m_ShellConstants.add("RECENT",CSIDL_RECENT, CSIDL_RECENT);
  m_ShellConstants.add("FAVORITES",CSIDL_FAVORITES, CSIDL_COMMON_FAVORITES);
  m_ShellConstants.add("MUSIC",CSIDL_MYMUSIC, CSIDL_COMMON_MUSIC);
  m_ShellConstants.add("PICTURES",CSIDL_MYPICTURES, CSIDL_COMMON_PICTURES);
  m_ShellConstants.add("VIDEOS",CSIDL_MYVIDEO, CSIDL_COMMON_VIDEO);
  m_ShellConstants.add("NETHOOD", CSIDL_NETHOOD, CSIDL_NETHOOD);
  m_ShellConstants.add("FONTS", CSIDL_FONTS, CSIDL_FONTS);
  m_ShellConstants.add("TEMPLATES", CSIDL_TEMPLATES, CSIDL_COMMON_TEMPLATES);
  m_ShellConstants.add("APPDATA", CSIDL_APPDATA, CSIDL_COMMON_APPDATA);
  m_ShellConstants.add("PRINTHOOD", CSIDL_PRINTHOOD, CSIDL_PRINTHOOD);
  //m_ShellConstants.add("ALTSTARTUP", CSIDL_ALTSTARTUP, CSIDL_COMMON_ALTSTARTUP);
  m_ShellConstants.add("INTERNET_CACHE", CSIDL_INTERNET_CACHE, CSIDL_INTERNET_CACHE);
  m_ShellConstants.add("COOKIES", CSIDL_COOKIES, CSIDL_COOKIES);
  m_ShellConstants.add("HISTORY", CSIDL_HISTORY, CSIDL_HISTORY);
  m_ShellConstants.add("PROFILE", CSIDL_PROFILE, CSIDL_PROFILE);
  m_ShellConstants.add("ADMINTOOLS", CSIDL_ADMINTOOLS, CSIDL_COMMON_ADMINTOOLS);
  m_ShellConstants.add("RESOURCES", CSIDL_RESOURCES, CSIDL_RESOURCES);
  m_ShellConstants.add("RESOURCES_LOCALIZED", CSIDL_RESOURCES_LOCALIZED, CSIDL_RESOURCES_LOCALIZED);
  m_ShellConstants.add("CDBURN_AREA", CSIDL_CDBURN_AREA, CSIDL_CDBURN_AREA);
}

namespace {
string get_executable_path(const char* argv0) {
#ifdef _WIN32
  char temp_buf[1024];
  temp_buf[0] = '\0';
  int rc = GetModuleFileName(NULL,temp_buf,1024);
  assert(rc != 0);
  return string(temp_buf);
#else
  return get_full_path(argv0);
#endif
}

string get_executable_dir(const char *argv0) {
  return get_dir_name(get_executable_path(argv0));
}

} // end anonymous namespace

void CEXEBuild::setdirs(const char *argv0)
{
  string nsis_dir = get_executable_dir(argv0);

  definedlist.add("NSISDIR",nsis_dir.c_str());

  nsis_dir += PLATFORM_PATH_SEPARATOR_STR;
  nsis_dir += "Include";
  include_dirs.add(nsis_dir.c_str(),0);
}


int CEXEBuild::getcurdbsize() { return cur_datablock->getlen(); }

// returns offset in stringblock
int CEXEBuild::add_string(const char *string, int process/*=1*/, WORD codepage/*=CP_ACP*/)
{
  if (!string || !*string) return 0;

  if (*string == '$' && *(string+1) == '(') {
    int idx = 0;
    char *cp = strdup(string+2);
    char *p = strchr(cp, ')');
    if (p && p[1] == '\0' ) { // if string is only a language str identifier
      *p = 0;
      idx = DefineLangString(cp, process);
    }
    free(cp);
    if (idx < 0) return idx;
  }

  if (!process) return cur_strlist->add(string,2);

  char buf[NSIS_MAX_STRLEN*4];
  preprocess_string(buf,string,codepage);
  return cur_strlist->add(buf,2);
}

int CEXEBuild::add_intstring(const int i) // returns offset in stringblock
{
  char i_str[128];
#ifdef _WIN32
  wsprintf(i_str, "%d", i);
#else
  snprintf(i_str, 128, "%d", i);
#endif
  return add_string(i_str);
}

// based on Dave Laundon's code
int CEXEBuild::preprocess_string(char *out, const char *in, WORD codepage/*=CP_ACP*/)
{
  const char *p=in;
  while (*p)
  {
    const char *np;
#ifdef _WIN32
    np = CharNextExA(codepage, p, 0);
#else
    {
      char buf[1024];
      snprintf(buf, 1024, "CP%d", codepage);
      setlocale(LC_CTYPE, buf);
      int len = mblen(p, strlen(p));
      if (len > 0)
        np = p + len;
      else
        np = p + 1;
      setlocale(LC_CTYPE, "");
    }
#endif
    if (np - p > 1) // multibyte char
    {
      int l = np - p;
      while (l--)
      {
        int i = (unsigned char)*p++;
        if (i >= NS_CODES_START) {
          *out++ = (char)NS_SKIP_CODE;
        }
        *out++=i;
      }
      continue;
    }

    int i = (unsigned char)*p;

    p=np;
    
    // Test for characters extending into the variable codes
    if (i >= NS_CODES_START) {
      *out++ = (char)NS_SKIP_CODE;
    }
    else if (i == '$')
    {
      if (*p == '$')
        p++; // Can simply convert $$ to $ now
      else
      {
        {
          bool bProceced=false;
          if ( *p )
          {
            const char *pUserVarName = p;
            while (isSimpleChar(*pUserVarName))
              pUserVarName++;

            while (pUserVarName > p)
            {
              if (m_ShellConstants.get((char*)p, pUserVarName-p) >= 0)
                break; // Upps it's a shell constant

              int idxUserVar = m_UserVarNames.get((char*)p, pUserVarName-p);
              if (idxUserVar >= 0)
              {
                // Well, using variables inside string formating doens't mean 
                // using the variable, beacuse it will be always an empty string
                // which is also memory wasting
                // So the line below must be commented !??
                //m_UserVarNames.inc_reference(idxUserVar);
                *out++ = (unsigned int) NS_VAR_CODE; // Named user variable;
                *(WORD*)out = CODE_SHORT(idxUserVar);
                out += sizeof(WORD);
                p += pUserVarName-p;
                bProceced = true;
                break;
              }
              pUserVarName--;
            }
          }
          if (!bProceced && *p)
          {
            const char *pShellConstName = p;
            while (isSimpleChar(*pShellConstName))
              pShellConstName++;

            while (pShellConstName > p)
            {
              int idxConst = m_ShellConstants.get((char*)p, pShellConstName - p);
              if (idxConst >= 0)
              {
                int CSIDL_Value_current = m_ShellConstants.get_value1(idxConst);
                int CSIDL_Value_all = m_ShellConstants.get_value2(idxConst);
                *out++=(unsigned int)NS_SHELL_CODE; // Constant code identifier
                *out++=(char)CSIDL_Value_current;
                *out++=(char)CSIDL_Value_all;
                p = pShellConstName;
                bProceced = true;
                break;
              }
              pShellConstName--;
            }
          }
          if ( !bProceced && *p == '(' )
          {
            int idx = -1;
            char *cp = strdup(p+1);
            char *pos = strchr(cp, ')');
            if (pos) 
            {
              *pos = 0;
              idx = DefineLangString(cp);
              if (idx < 0)
              {
                *out++ = (unsigned int)NS_LANG_CODE; // Next word is lang-string Identifier
                *(WORD*)out= CODE_SHORT(-idx-1);
                out += sizeof(WORD);
                p += strlen(cp) + 2;
                bProceced = true;
              }
            }
            free(cp);              
          }
          if ( bProceced )
            continue;
          else
          {
            char tbuf[64];
            char cBracket = '\0';
            bool bDoWarning = true;

            if ( *p == '[' )
              cBracket = ']';
            else if ( *p == '(' )
              cBracket = ')';
            else if ( *p == '{' )
              cBracket = '}';
            
            strncpy(tbuf,p,63);
            tbuf[63]=0;
            
            if ( cBracket != 0 )
            {
              if (strchr(tbuf,cBracket)) (strchr(tbuf,cBracket)+1)[0]=0;
              if ( tbuf[0] == '{' && tbuf[strlen(tbuf)-1] == '}' )
              {
                char *tstIfDefine = strdup(tbuf+1);
                tstIfDefine[strlen(tstIfDefine)-1] = '\0';
                bDoWarning = definedlist.find(tstIfDefine) == NULL;
              }
            }
            else
            {
              if (strstr(tbuf," ")) strstr(tbuf," ")[0]=0;
            }
            if ( bDoWarning )
              warning_fl("unknown variable/constant \"%s\" detected, ignoring",tbuf);
            i = '$';
          }
        }
      }
    }
    *out++=i;
  }
  *out=0;
  return 0;
}

// what it does is, when you pass it the offset of the last item added, it will determine if
// that data is already present in the datablock, and if so, reference it instead (and shorten
// the datablock as necessary). Reduces overhead if you want to add files to a couple places.
// Woo, an optimizing installer generator, now we're styling.

int CEXEBuild::datablock_optimize(int start_offset, int first_int)
{
  int this_len = cur_datablock->getlen() - start_offset;

  cached_db_size this_size = {first_int, start_offset};
  cur_datablock_cache->add(&this_size, sizeof(cached_db_size));

  if (!build_optimize_datablock || this_len < (int) sizeof(int))
    return start_offset;

  MMapBuf *db = (MMapBuf *) cur_datablock;
  db->setro(TRUE);

  cached_db_size *db_sizes = (cached_db_size *) cur_datablock_cache->get();
  int db_sizes_num = cur_datablock_cache->getlen() / sizeof(cached_db_size);
  db_sizes_num--; // don't compare with the one we just added

  for (int i = 0; i < db_sizes_num; i++)
  {
    if (db_sizes[i].first_int == first_int)
    {
      int pos = db_sizes[i].start_offset;
      int left = this_len;
      while (left > 0)
      {
        int l = min(left, build_filebuflen);
        int la = l;
        void *newstuff = db->get(start_offset + this_len - left, l);
        void *oldstuff = db->getmore(pos + this_len - left, &la);

        int res = memcmp(newstuff, oldstuff, l);

        db->release(oldstuff, la);
        db->release();

        if (res)
        {
          break;
        }

        left -= l;
      }

      if (!left)
      {
        db_opt_save += this_len;
        db->resize(max(start_offset, pos + this_len));
        db->setro(FALSE);
        cur_datablock_cache->resize(cur_datablock_cache->getlen() - sizeof(cached_db_size));
        return pos;
      }
    }
  }

  db->setro(FALSE);

  return start_offset;
}

int CEXEBuild::add_db_data(IMMap *map) // returns offset
{
  build_compressor_set = true;

  int done = 0;

  if (!map)
  {
    ERROR_MSG("Error: add_db_data() called with invalid mapped file\n");
    return -1;
  }

  int length = map->getsize();

  if (length < 0)
  {
    ERROR_MSG("Error: add_db_data() called with length=%d\n", length);
    return -1;
  }

  MMapBuf *db = (MMapBuf *) cur_datablock;

  int st = db->getlen();

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  if (length && !build_compress_whole && build_compress)
  {
    // grow datablock so that there is room to compress into
    int bufferlen = length + 1024 + length / 4; // give a nice 25% extra space
    db->resize(st + bufferlen + sizeof(int));

    int n;
    if (compressor == &lzma_compressor)
      n = ((CLZMA *) compressor)->Init(build_compress_level, build_compress_dict_size);
    else
      n = compressor->Init(build_compress_level);
    if (n != C_OK)
    {
      ERROR_MSG("Internal compiler error #12345: deflateInit() failed(%d - %s).\n", n, compressor->GetErrStr(n));
      extern void quit(); quit();
    }

    int avail_in = length;
    int avail_out = bufferlen;
    int ret;
    while (avail_in > 0)
    {
      int in_len = min(build_filebuflen, avail_in);
      int out_len = min(build_filebuflen, avail_out);
      
      compressor->SetNextIn((char *) map->get(length - avail_in, in_len), in_len);
      compressor->SetNextOut((char *) db->get(st + sizeof(int) + bufferlen - avail_out, out_len), out_len);
      if ((ret = compressor->Compress(0)) < 0)
      {
        ERROR_MSG("Error: add_db_data() - compress() failed(%d - %s)\n", ret, compressor->GetErrStr(ret));
        return -1;
      }
      map->release();
      db->flush(out_len);
      db->release();
      avail_in -= in_len - compressor->GetAvailIn();
      avail_out -= out_len - compressor->GetAvailOut();

      if (!avail_out)
        // not enough space in the output buffer - no compression is better
        break;
    }

    // if not enough space in the output buffer - no compression is better
    if (avail_out)
    {
      char *out;

      char a;
      compressor->SetNextIn(&a,0);

      do
      {
        int out_len = min(build_filebuflen, avail_out);

        out = (char *) db->get(st + sizeof(int) + bufferlen - avail_out, out_len);

        compressor->SetNextOut(out, out_len);
        if ((ret = compressor->Compress(C_FINISH)) < 0)
        {
          ERROR_MSG("Error: add_db_data() - compress() failed(%d - %s)\n", ret, compressor->GetErrStr(ret));
          return -1;
        }

        db->flush(out_len);
        db->release();

        avail_out -= out_len - compressor->GetAvailOut();
      }
      while (compressor->GetNextOut() - out > 0 && avail_out > 0);

      compressor->End();

      int used = bufferlen - avail_out;

      // never store compressed if output buffer is full (compression increased the size...)
      if (avail_out && (build_compress == 2 || used < length))
      {
        done=1;
        db->resize(st + used + sizeof(int));

        *(int*)db->get(st, sizeof(int)) = used | 0x80000000;
        db->release();

        int nst = datablock_optimize(st, used | 0x80000000);
        if (nst == st) db_comp_save += length - used;
        else st = nst;
      }
    }
    else
      compressor->End();
  }
#endif // NSIS_CONFIG_COMPRESSION_SUPPORT

  if (!done)
  {
    db->resize(st + length + sizeof(int));
    int *plen = (int *) db->get(st, sizeof(int));
    *plen = length;
    db->release();

    int left = length;
    while (left > 0)
    {
      int l = min(build_filebuflen, left);
      int *p = (int *) db->get(st + sizeof(int) + length - left, l);
      memcpy(p, map->get(length - left, l), l);
      db->flush(l);
      db->release();
      map->release();
      left -= l;
    }

    st = datablock_optimize(st, length);
  }

  db_full_size += length + sizeof(int);

  return st;
}

int CEXEBuild::add_db_data(const char *data, int length) // returns offset
{
  MMapFake fakemap;
  fakemap.set(data, length);
  return add_db_data(&fakemap);
}

int CEXEBuild::add_data(const char *data, int length, IGrowBuf *dblock) // returns offset
{
  build_compressor_set=true;

  int done=0;

  if (length < 0)
  {
    ERROR_MSG("Error: add_data() called with length=%d\n",length);
    return -1;
  }

  int st=dblock->getlen();

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  if (!build_compress_whole && build_compress)
  {
    // grow datablock so that there is room to compress into
    int bufferlen=length+1024+length/4; // give a nice 25% extra space
    dblock->resize(st+bufferlen+sizeof(int));

    int n;
    if (compressor == &lzma_compressor)
      n = ((CLZMA *) compressor)->Init(build_compress_level, build_compress_dict_size);
    else
      n = compressor->Init(build_compress_level);
    if (n != C_OK)
    {
      ERROR_MSG("Internal compiler error #12345: deflateInit() failed(%d - %s).\n", n, compressor->GetErrStr(n));
      extern void quit(); quit();
    }

    compressor->SetNextIn((char*)data, length);
    compressor->SetNextOut((char*)dblock->get() + st + sizeof(int), bufferlen);

    compressor->Compress(C_FINISH);

    int used=bufferlen-compressor->GetAvailOut();

    // never store compressed if output buffer is full
    if (compressor->GetAvailOut() && (build_compress == 2 || used < length))
    {
      done=1;
      dblock->resize(st+used+sizeof(int));

      *((int*)((char *)dblock->get()+st)) = used|0x80000000;
    }
    compressor->End();
  }
#endif // NSIS_CONFIG_COMPRESSION_SUPPORT

  if (!done)
  {
    dblock->resize(st);
    dblock->add(&length,sizeof(int));
    dblock->add(data,length);
  }

  return st;
}

int CEXEBuild::add_label(const char *name)
{
  if (!build_cursection)
  {
    ERROR_MSG("Error: Label declaration not valid outside of function/section\n");
    return PS_ERROR;
  }
  if ((name[0] >= '0' && name[0] <= '9') || name[0] == '-' || name[0] == ' ' || name[0] == ':')
  {
    ERROR_MSG("Error: labels must not begin with 0-9, -, :, or a space.\n");
    return PS_ERROR;
  }

  int cs=build_cursection->code;
  int ce=cs+build_cursection->code_size;

  char *p=strdup(name);
  if (p[strlen(p)-1] == ':') p[strlen(p)-1]=0;
  int offs=ns_label.add(p,0);
  free(p);

  int n=cur_labels->getlen()/sizeof(section);
  if (n)
  {
    section *t=(section*)cur_labels->get();
    while (n--)
    {
      if ((*name == '.' || (t->code >= cs && t->code <= ce))  &&
          t->name_ptr==offs)
      {
        if (*name == '.') ERROR_MSG("Error: global label \"%s\" already declared\n",name);
        else ERROR_MSG("Error: label \"%s\" already declared in section/function\n",name);
        return PS_ERROR;
      }
      t++;
    }
  }

  section s={0};
  s.name_ptr = offs;
  s.code = ce;
  cur_labels->add(&s,sizeof(s));

  return PS_OK;
}

int CEXEBuild::add_function(const char *funname)
{
  if (build_cursection_isfunc)
  {
    ERROR_MSG("Error: Function open when creating function (use FunctionEnd first)\n");
    return PS_ERROR;
  }
  if (build_cursection)
  {
    ERROR_MSG("Error: Section open when creating function (use SectionEnd first)\n");
    return PS_ERROR;
  }
  if (cur_page)
  {
    ERROR_MSG("Error: PageEx open when creating function (use PageExEnd first)\n");
    return PS_ERROR;
  }
  if (!funname[0])
  {
    ERROR_MSG("Error: Function must have a name\n");
    return PS_ERROR;
  }

  set_uninstall_mode(!strnicmp(funname,"un.",3));

  int addr=ns_func.add(funname,0);
  int x;
  int n=cur_functions->getlen()/sizeof(section);
  section *tmp=(section*)cur_functions->get();
  for (x = 0; x < n; x ++)
  {
    if (tmp[x].name_ptr == addr)
  {
    ERROR_MSG("Error: Function named \"%s\" already exists.\n",funname);
    return PS_ERROR;
  }
  }

  cur_functions->resize((n+1)*sizeof(section));
  build_cursection=((section*)cur_functions->get())+n;
  build_cursection_isfunc=1;
  build_cursection->name_ptr=addr;
  build_cursection->code=cur_entries->getlen()/sizeof(entry);
  build_cursection->code_size=0;
  build_cursection->install_types=0;
  build_cursection->flags=0;
  build_cursection->size_kb=0;
  return PS_OK;
}

int CEXEBuild::function_end()
{
  if (!build_cursection_isfunc)
  {
    ERROR_MSG("Error: No function open, FunctionEnd called\n");
    return PS_ERROR;
  }
  // add ret.
  add_entry_direct(EW_RET);

  build_cursection_isfunc=0;
  build_cursection=NULL;

  set_uninstall_mode(0);
  return PS_OK;
}


int CEXEBuild::section_add_flags(int flags)
{
  if (!build_cursection || build_cursection_isfunc)
  {
    ERROR_MSG("Error: can't modify flags when no section is open\n");
    return PS_ERROR;
  }
  build_cursection->flags |= flags;
  return PS_OK;
}

int CEXEBuild::section_add_install_type(int inst_type)
{
  if (!build_cursection || build_cursection_isfunc)
  {
    ERROR_MSG("Error: can't modify flags when no section is open\n");
    return PS_ERROR;
  }
  if (build_cursection->install_types == ~0)
    build_cursection->install_types = 0;
  build_cursection->install_types |= inst_type;
  return PS_OK;
}

void CEXEBuild::section_add_size_kb(int kb)
{
  if (build_cursection)
  {
    build_cursection->size_kb+=kb;
  }
}

int CEXEBuild::section_end()
{
  if (build_cursection_isfunc)
  {
    ERROR_MSG("Error: SectionEnd specified in function (not section)\n");
    return PS_ERROR;
  }
  if (!build_cursection)
  {
    ERROR_MSG("Error: SectionEnd specified and no sections open\n");
    return PS_ERROR;
  }
  add_entry_direct(EW_RET);
  build_cursection->code_size--;
  build_cursection=NULL;
  if (!subsection_open_cnt)
    set_uninstall_mode(0);
  return PS_OK;
}

int CEXEBuild::add_section(const char *secname, const char *defname, int expand/*=0*/)
{
  if (build_cursection_isfunc)
  {
    ERROR_MSG("Error: Section can't create section (already in function, use FunctionEnd first)\n");
    return PS_ERROR;
  }
  if (cur_page) {
    ERROR_MSG("Error: PageEx already open, call PageExEnd first\n");
    return PS_ERROR;
  }
  if (build_cursection)
  {
    ERROR_MSG("Error: Section already open, call SectionEnd first\n");
    return PS_ERROR;
  }

  section new_section;
  new_section.flags = SF_SELECTED;
  new_section.flags |= expand ? SF_EXPAND : 0;
  new_section.code_size = 0;
  new_section.size_kb = 0;

  char *name = (char*)secname;

  if (secname[0] == '-')
  {
    if (secname[1])
    {
      new_section.flags |= SF_SUBSEC;
      name++;
    }
    else
      new_section.flags |= SF_SUBSECEND;
  }

  if (name[0] == '!')
  {
    name++;
    new_section.flags |= SF_BOLD;
  }

  int old_uninstall_mode = uninstall_mode;

  set_uninstall_mode(0);

  if (!strnicmp(name, "un.", 3))
  {
    set_uninstall_mode(1);
    name += 3;
  }

  if (!stricmp(name, "uninstall"))
  {
    set_uninstall_mode(1);
  }

  if ((new_section.flags & SF_SUBSECEND) && subsection_open_cnt && old_uninstall_mode)
  {
    set_uninstall_mode(1);
  }

  if (subsection_open_cnt)
  {
    if (uninstall_mode != old_uninstall_mode)
    {
      ERROR_MSG("Error: Can't create %s section in %s subsection (use SubSectionEnd first)\n", uninstall_mode ? "uninstaller" : "installer", old_uninstall_mode ? "uninstaller" : "installer");
      return PS_ERROR;
    }
  }

  new_section.code = cur_entries->getlen() / sizeof(entry);

  new_section.install_types = *name ? 0 : ~0;
  new_section.name_ptr = add_string(name);

  cur_sections->add(&new_section, sizeof(section));
  build_cursection = (section *) cur_sections->get() + cur_header->blocks[NB_SECTIONS].num;

  if (defname[0])
  {
    char buf[128];
#ifdef _WIN32
    wsprintf(buf, "%d", cur_header->blocks[NB_SECTIONS].num);
#else
    snprintf(buf, 128, "%d", cur_header->blocks[NB_SECTIONS].num);
#endif
    if (definedlist.add(defname, buf))
    {
      ERROR_MSG("Error: \"%s\" already defined, can't assign section index!\n", defname);
      return PS_ERROR;
    }
  }

  cur_header->blocks[NB_SECTIONS].num++;

  if (new_section.flags & (SF_SUBSEC | SF_SUBSECEND))
  {
    add_entry_direct(EW_RET);
    build_cursection->code_size = 0;

    build_cursection = 0;

    if (new_section.flags & SF_SUBSECEND)
    {
      subsection_open_cnt--;
      if (subsection_open_cnt < 0)
      {
        ERROR_MSG("SubSectionEnd: no SubSections are open\n");
        return PS_ERROR;
      }
      if (!subsection_open_cnt)
      {
        set_uninstall_mode(0);
      }
    }
    else
      subsection_open_cnt++;
  }

  return PS_OK;
}

int CEXEBuild::add_entry(const entry *ent)
{
  if (!build_cursection && !uninstall_mode)
  {
    ERROR_MSG("Error: Can't add entry, no section or function is open!\n");
    return PS_ERROR;
  }

  cur_entries->add(ent,sizeof(entry));
  cur_instruction_entry_map->add(&multiple_entries_instruction,sizeof(int));
  build_cursection->code_size++;
  cur_header->blocks[NB_ENTRIES].num++;

  multiple_entries_instruction=1;

  return PS_OK;
}

int CEXEBuild::add_entry_direct(int which, int o0, int o1, int o2, int o3, int o4, int o5 /*o#=0*/)
{
  entry ent;
  ent.which = which;
  ent.offsets[0] = o0;
  ent.offsets[1] = o1;
  ent.offsets[2] = o2;
  ent.offsets[3] = o3;
  ent.offsets[4] = o4;
  ent.offsets[5] = o5;
  return add_entry(&ent);
}

int CEXEBuild::resolve_jump_int(const char *fn, int *a, int offs, int start, int end)
{
  if (*a > 0)
  {
    char *lname=(char*)ns_label.get()+*a;
    if (lname[0] == '-' || lname[0]=='+')
    {
      int jump = atoi(lname);
      int *skip_map = (int *) cur_instruction_entry_map->get();

      int direction = 1;
      if (jump < 0)
        direction = -1;

      for (; jump != 0; jump -= direction)
      {
        offs += direction;
        if (offs >= 0 && offs < cur_instruction_entry_map->getlen() * (int) sizeof(int))
        {
          while (skip_map[offs])
          {
            offs += direction;
          }
        }
      }

      *a = offs + 1;
    }
    else
    {
      section *s = (section*)cur_labels->get();
      int n=cur_labels->getlen()/sizeof(section);
      while (n-->0)
      {
        if ((*lname == '.' || (s->code >= start && s->code <= end)) && s->name_ptr == *a)
        {
          *a = s->code+1;     // jumps are to the absolute position, +1 (to differentiate between no jump, and jumping to offset 0)
          s->flags++;
          return 0;
        }
        s++;
      }

      ERROR_MSG("Error: could not resolve label \"%s\" in %s\n",lname,fn);
      return 1;
    }
  }
  else if (*a < 0) // to jump to a user variable target, -variable_index-1 is already stored.
  {
  }
  // otherwise, *a is 0, which means no jump and we also leave it intact
  return 0;
}

int CEXEBuild::resolve_call_int(const char *fn, const char *str, int fptr, int *ofs)
{
  if (fptr < 0) return 0;
  int nf=cur_functions->getlen()/sizeof(section);
  section *sec=(section *)cur_functions->get();
  while (nf-- > 0)
  {
    if (sec->name_ptr>0 && sec->name_ptr == fptr)
    {
      ofs[0]=sec->code;
      sec->flags++;
      return 0;
    }
    sec++;
  }
  ERROR_MSG("Error: resolving %s function \"%s\" in %s\n",str,(char*)ns_func.get()+fptr,fn);
  ERROR_MSG("Note: uninstall functions must begin with \"un.\", and install functions must not\n");
  return 1;
}

int CEXEBuild::resolve_instruction(const char *fn, const char *str, entry *w, int offs, int start, int end)
{
  if (w->which == EW_NOP)
  {
    if (resolve_jump_int(fn,&w->offsets[0],offs,start,end)) return 1;
  }
#ifdef NSIS_SUPPORT_MESSAGEBOX
  else if (w->which == EW_MESSAGEBOX)
  {
    if (resolve_jump_int(fn,&w->offsets[3],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[5],offs,start,end)) return 1;
  }
#endif
  else if (w->which == EW_IFFILEEXISTS)
  {
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[2],offs,start,end)) return 1;
  }
  else if (w->which == EW_IFFLAG)
  {
    if (resolve_jump_int(fn,&w->offsets[0],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
  }
#ifdef NSIS_SUPPORT_STROPTS
  else if (w->which == EW_STRCMP)
  {
    if (resolve_jump_int(fn,&w->offsets[2],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[3],offs,start,end)) return 1;
  }
#endif
#ifdef NSIS_SUPPORT_INTOPTS
  else if (w->which == EW_INTCMP)
  {
    if (resolve_jump_int(fn,&w->offsets[2],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[3],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[4],offs,start,end)) return 1;
  }
#endif
#ifdef NSIS_SUPPORT_HWNDS
  else if (w->which == EW_ISWINDOW)
  {
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[2],offs,start,end)) return 1;
  }
#endif
  else if (w->which == EW_CALL)
  {
    if (w->offsets[0] >= 0 && w->offsets[1]) // get as jump
    {
      if (resolve_jump_int(fn,&w->offsets[0],offs,start,end)) return 1;
    }
    else
    {
      if (w->offsets[0] >= 0 && resolve_call_int(fn,str,w->offsets[0],w->offsets)) return 1;
      // if w->offsets[0] >= 0, EW_CALL requires that it 1-based.
      // otherwise, if < 0, it needs an increment anyway (since it
      // was encoded with a -2 base, to prevent it looking like an
      // empty string "")
      w->offsets[0]++;
    }
  }
#ifdef NSIS_SUPPORT_STROPTS
  else if (w->which == EW_GETFUNCTIONADDR)
  {
    if (w->offsets[1] < 0)
    {
      ERROR_MSG("Error: GetFunctionAddress requires a real function to get address of.\n");
      return 1;
    }

    if (resolve_call_int(fn,str,w->offsets[1],&w->offsets[1])) return 1;

    w->which=EW_ASSIGNVAR;
    w->offsets[1]=add_intstring(w->offsets[1]+1); // +1 here to make 1-based.
  }
  else if (w->which == EW_GETLABELADDR)
  {
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
    w->which=EW_ASSIGNVAR;
    w->offsets[1]=add_intstring(w->offsets[1]);
  }
#endif
  return 0;
}

int CEXEBuild::resolve_coderefs(const char *str)
{
  // resolve jumps&calls
  {
    section *sec=(section *)cur_functions->get();
    int l=cur_functions->getlen()/sizeof(section);
    entry *w=(entry *)cur_entries->get();
    while (l-- > 0)
    {
      int x;
      for (x = sec->code; x < sec->code+sec->code_size; x ++)
      {
        char fname[1024];
        wsprintf(fname,"function \"%s\"",ns_func.get()+sec->name_ptr);
        if (resolve_instruction(fname,str,w+x,x,sec->code,sec->code+sec->code_size)) return 1;
      }
      sec++;
    }

    int cnt=0;
    sec=(section *)cur_sections->get();
    l=cur_sections->getlen()/sizeof(section);
    while (l-- > 0)
    {
      int x=sec->name_ptr;
      char fname[1024];
      const char *section_name;
      if (x < 0)
      {
        // lang string
        section_name = "$(lang string)";
      }
      else
      {
        // normal string
        section_name = cur_strlist->get() + x;
      }
      if (x) wsprintf(fname,"%s section \"%s\" (%d)",str,section_name,cnt);
      else wsprintf(fname,"unnamed %s section (%d)",str,cnt);
      for (x = sec->code; x < sec->code+sec->code_size; x ++)
      {
        if (resolve_instruction(fname,str,w+x,x,sec->code,sec->code+sec->code_size))
          return 1;
      }
      sec++;
      cnt++;
    }
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_SUPPORT_CODECALLBACKS
    if (cur_pages->getlen()) {
      page *p=(page *)cur_pages->get();
      int i = 0;
      while (i < cur_header->blocks[NB_PAGES].num) {
        char pagestr[1024];
        wsprintf(pagestr, "%s pages", str);
        if (resolve_call_int(pagestr,p->dlg_id?"pre-page":"create-page",p->prefunc,&p->prefunc)) return 1;
        if (resolve_call_int(pagestr,"show-page",p->showfunc,&p->showfunc)) return 1;
        if (resolve_call_int(pagestr,"leave-page",p->leavefunc,&p->leavefunc)) return 1;
        p++;
        i++;
      }
    }
#endif
#endif
  }

#ifdef NSIS_SUPPORT_CODECALLBACKS
  // resolve callbacks
  {
    struct {
      char *name;
      int *p;
    } callbacks[] = {
      {"%s.onInit", &cur_header->code_onInit},
      {"%s.on%sInstSuccess", &cur_header->code_onInstSuccess},
      {"%s.on%sInstFailed", &cur_header->code_onInstFailed},
      {"%s.onUserAbort", &cur_header->code_onUserAbort},
      {"%s.onVerifyInstDir", &cur_header->code_onVerifyInstDir},
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
      {"%s.onGUIInit", &cur_header->code_onGUIInit},
      {"%s.onGUIEnd", &cur_header->code_onGUIEnd},
      {"%s.onMouseOverSection", &cur_header->code_onMouseOverSection},
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
      {"%s.onSelChange", &cur_header->code_onSelChange},
#endif
      {0, 0}
    };

    for (int i = 0; callbacks[i].name; i++) {
      const char *un = uninstall_mode ? "un" : "";
      char fname[1024];
      wsprintf(fname, callbacks[i].name, un, un);
      char cbstr[1024];
      wsprintf(cbstr, "%s callback", str);
      char cbstr2[1024];
      wsprintf(cbstr2, "%s.callbacks", un);

      if (resolve_call_int(cbstr,cbstr2,ns_func.find(fname,0),callbacks[i].p))
        return PS_ERROR;
    }
  }
#endif//NSIS_SUPPORT_CODECALLBACKS

  // optimize unused functions
  {
    section *sec=(section *)cur_functions->get();
    int l=cur_functions->getlen()/sizeof(section);
    entry *w=(entry*)cur_entries->get();
    while (l-- > 0)
    {
      if (sec->name_ptr)
      {
        if (!sec->flags)
        {
          if (sec->code_size>0)
          {
            warning("%s function \"%s\" not referenced - zeroing code (%d-%d) out\n",str,
              ns_func.get()+sec->name_ptr,
              sec->code,sec->code+sec->code_size);
            memset(w+sec->code,0,sec->code_size*sizeof(entry));
          }
        }
      }
      sec++;
    }
  }

  // give warnings on unused labels
  {
    section *t=(section*)cur_labels->get();
    int n=cur_labels->getlen()/sizeof(section);
    while (n-->0)
    {
      if (!t->flags)
      {
        char *n=(char*)ns_label.get()+t->name_ptr;
        if (*n == '.') warning("global label \"%s\" not used",n);
        else warning("label \"%s\" not used",n);
      }
      t++;
    }
  }

  return 0;
}

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
int CEXEBuild::add_page(int type)
{
  page pg = {
    0,
    0,
#ifdef NSIS_SUPPORT_CODECALLBACKS
    -1,
    -1,
    -1,
#endif
    0,
  };

#ifndef NSIS_CONFIG_LICENSEPAGE
  if (type == PAGE_LICENSE)
  {
    ERROR_MSG("Error: can't add license page, NSIS_CONFIG_LICENSEPAGE not defined.\n");
    return PS_ERROR;
  }
#endif
#ifndef NSIS_CONFIG_COMPONENTPAGE
  if (type == PAGE_COMPONENTS)
  {
    ERROR_MSG("Error: can't add components page, NSIS_CONFIG_COMPONENTPAGE not defined.\n");
    return PS_ERROR;
  }
#endif
#ifndef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (type == PAGE_COMPONENTS)
  {
    ERROR_MSG("Error: can't add uninstConfirm page, NSIS_CONFIG_UNINSTALL_SUPPORT not defined.\n");
    return PS_ERROR;
  }
#endif

  struct {
    int wndproc_id;
    int dlg_id;
  } ids[] = {
    {PWP_CUSTOM, 0}, // custom
#ifdef NSIS_CONFIG_LICENSEPAGE
    {PWP_LICENSE, IDD_LICENSE}, // license
#else
    {0, IDD_LICENSE}, // license
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
    {PWP_SELCOM, IDD_SELCOM}, // components
#else
    {0, IDD_SELCOM}, // components
#endif
    {PWP_DIR, IDD_DIR}, // directory
    {PWP_INSTFILES, IDD_INSTFILES}, // instfiles
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    {PWP_UNINST, IDD_UNINST}, // uninstConfirm
#else
    {0, IDD_UNINST}, // uninstConfirm
#endif
    {PWP_COMPLETED, -1} // completed
  };

  pg.wndproc_id = ids[type].wndproc_id;
  pg.dlg_id = ids[type].dlg_id;

  cur_pages->add(&pg,sizeof(page));

  cur_page = (page *)cur_pages->get() + cur_header->blocks[NB_PAGES].num++;

  cur_page_type = type;

  return PS_OK;
}

int CEXEBuild::page_end()
{
  cur_page = 0;

  return PS_OK;
}
#endif

#ifdef NSIS_SUPPORT_VERSION_INFO
int CEXEBuild::AddVersionInfo()
{
  GrowBuf VerInfoStream;

  if ( rVersionInfo.GetStringTablesCount() > 0 )
  {
    if ( !version_product_v[0] )
    {
      ERROR_MSG("Error: VIProductVersion is required when other version information functions are used.\n");
      return PS_ERROR;
    }
    else
    {
      int imm, iml, ilm, ill;
      if ( sscanf(version_product_v, "%d.%d.%d.%d", &imm, &iml, &ilm, &ill) != 4 )
      { 
        ERROR_MSG("Error: invalid VIProductVersion format, should be X.X.X.X\n");
        return PS_ERROR;
      }
      rVersionInfo.SetFileVersion(MAKELONG(iml, imm),MAKELONG(ill, ilm));
      rVersionInfo.SetProductVersion(MAKELONG(iml, imm),MAKELONG(ill, ilm));

      init_res_editor();
      for ( int i = 0; i < rVersionInfo.GetStringTablesCount(); i++ )
      {
        LANGID lang_id = rVersionInfo.GetLangID(i);
        int code_page = rVersionInfo.GetCodePage(i);
        LanguageTable *Table = GetLangTable(lang_id);

        if ( !rVersionInfo.FindKey(lang_id, code_page, "FileVersion") )
          warning("Generating version information for language \"%04d-%s\" without standard key \"FileVersion\"", lang_id, Table->nlf.m_bLoaded ? Table->nlf.m_szName : lang_id == 1033 ? "English" : "???");
        if ( !rVersionInfo.FindKey(lang_id, code_page, "FileDescription") )
          warning("Generating version information for language \"%04d-%s\" without standard key \"FileDescription\"", lang_id, Table->nlf.m_bLoaded ? Table->nlf.m_szName : lang_id == 1033 ? "English" : "???");
        if ( !rVersionInfo.FindKey(lang_id, code_page, "LegalCopyright") )
          warning("Generating version information for language \"%04d-%s\" without standard key \"LegalCopyright\"", lang_id, Table->nlf.m_bLoaded ? Table->nlf.m_szName : lang_id == 1033 ? "English" : "???");

        rVersionInfo.ExportToStream(VerInfoStream, i);
        res_editor->UpdateResource(RT_VERSION, 1, lang_id, (BYTE*)VerInfoStream.get(), VerInfoStream.getlen());
      }
    }
  }
  
  return PS_OK;
}
#endif // NSIS_SUPPORT_VERSION_INFO

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
int CEXEBuild::ProcessPages()
{
  SCRIPT_MSG("Processing pages... ");

  int license_normal=0;
  int license_fsrb=0;
  int license_fscb=0;
  int selcom=0;
  int dir=0, dir_used;
  int uninstconfirm=0;
  int instlog=0, instlog_used;
  int main=0;

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
again:
#endif

  dir_used = 0;
  instlog_used = 0;

#ifdef NSIS_CONFIG_SILENT_SUPPORT
  if ((cur_header->flags & (CH_FLAGS_SILENT|CH_FLAGS_SILENT_LOG)) == 0)
#endif
  {
    main++;

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
#define LS(inst, uninst) (uninstall_mode ? uninst : inst)
#else
#define LS(inst, uninst) inst
#endif

    DefineInnerLangString(NLF_BRANDING);

    if (!cur_pages->getlen()) {
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (uninstall_mode) {
        if (HasUserDefined(NLF_UNINST_TEXT)) {
          add_page(PAGE_UNINSTCONFIRM);
          page_end();
        }
        add_page(PAGE_INSTFILES);
        page_end();
        add_page(PAGE_COMPLETED);
        page_end();
      }
      else
#endif
      {
#ifdef NSIS_CONFIG_LICENSEPAGE
        if (HasUserDefined(NLF_LICENSE_TEXT) && HasUserDefined(NLF_LICENSE_DATA)) {
          add_page(PAGE_LICENSE);
          page_end();
        }
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
        if (HasUserDefined(NLF_COMP_TEXT)) {
          add_page(PAGE_COMPONENTS);
          page_end();
        }
#endif
        if (HasUserDefined(NLF_DIR_TEXT)) {
          add_page(PAGE_DIRECTORY);
          page_end();
        }
        add_page(PAGE_INSTFILES);
        page_end();
        add_page(PAGE_COMPLETED);
        page_end();
      }
    }
    // start processing the pages
    {
      int i = 0;
      page *p = (page *) cur_pages->get();
      
      for (i = 0; i < cur_header->blocks[NB_PAGES].num; i++, p++) {
        page *pp = 0;

        if (i) {
          pp = p - 1;

          // set back button
          p->flags |= PF_BACK_SHOW;
          if (pp->wndproc_id != PWP_COMPLETED && p->wndproc_id != PWP_COMPLETED && p->wndproc_id != PWP_INSTFILES)
            p->flags |= PF_BACK_ENABLE;
          if (!p->back)
            p->back = DefineInnerLangString(NLF_BTN_BACK);

          // set previous page's next button
          if (!pp->next) {
            int str = 0;

#ifdef NSIS_CONFIG_LICENSEPAGE
            if (pp->wndproc_id == PWP_LICENSE && (!(pp->flags & PF_LICENSE_FORCE_SELECTION) || HasUserDefined(NLF_BTN_LICENSE)))
              str = NLF_BTN_LICENSE;
            else
#endif
            if (p->wndproc_id == PWP_INSTFILES)
              str = LS(NLF_BTN_INSTALL, NLF_BTN_UNINSTALL);
            else
              str = NLF_BTN_NEXT;

            pp->next = DefineInnerLangString(str);
          }

          // set previous page's click next text
          if (!pp->clicknext) {
            int str = 0;

            if (p->wndproc_id == PWP_INSTFILES)
              str = LS(NLF_CLICK_INSTALL, NLF_CLICK_UNINSTALL);
            else
              str = NLF_CLICK_NEXT;

            pp->clicknext = DefineInnerLangString(str);
          }
        }

        // enable next button
        if (p->wndproc_id != PWP_INSTFILES)
          p->flags |= PF_NEXT_ENABLE;

        // set cancel button
        if (!p->cancel)
          p->cancel = DefineInnerLangString(NLF_BTN_CANCEL);
        if (p->wndproc_id != PWP_INSTFILES && p->wndproc_id != PWP_COMPLETED)
          p->flags |= PF_CANCEL_ENABLE;

        // set caption
        struct {
          int caption;
          int ucaption;
        } captions[] = {
#ifdef NSIS_CONFIG_LICENSEPAGE
          {NLF_SUBCAPTION_LICENSE, NLF_SUBCAPTION_LICENSE},
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
          {NLF_SUBCAPTION_OPTIONS, NLF_SUBCAPTION_OPTIONS},
#endif
          {NLF_SUBCAPTION_DIR, NLF_SUBCAPTION_DIR},
          {NLF_SUBCAPTION_INSTFILES, NLF_USUBCAPTION_INSTFILES},
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
          {NLF_USUBCAPTION_CONFIRM, NLF_USUBCAPTION_CONFIRM},
#endif
          {NLF_SUBCAPTION_COMPLETED, NLF_USUBCAPTION_COMPLETED}
        };

        if (!p->caption && p->wndproc_id != PWP_CUSTOM) {
          p->caption = DefineInnerLangString(LS(captions[p->wndproc_id].caption, captions[p->wndproc_id].ucaption));
        }

        // set texts
        switch (p->dlg_id) {
#ifdef NSIS_CONFIG_LICENSEPAGE
          case IDD_LICENSE:
          case IDD_LICENSE_FSRB:
          case IDD_LICENSE_FSCB:
          {
            if (!(p->flags & PF_PAGE_EX))
              p->dlg_id = license_res_id;
            if (!(p->flags & (PF_LICENSE_FORCE_SELECTION | PF_LICENSE_NO_FORCE_SELECTION)))
              p->dlg_id = license_res_id;

            p->flags |= PF_NO_NEXT_FOCUS;

            if (!p->parms[1])
              p->parms[1] = DefineInnerLangString(NLF_LICENSE_DATA, 0);

            if (p->dlg_id == IDD_LICENSE) {
              if (!p->parms[0])
                p->parms[0] = DefineInnerLangString(LS(NLF_LICENSE_TEXT, NLF_ULICENSE_TEXT));

              license_normal++;
            }
            else if (p->dlg_id == IDD_LICENSE_FSCB) {
              p->flags |= PF_LICENSE_FORCE_SELECTION;

              if (!p->parms[0])
                p->parms[0] = DefineInnerLangString(LS(NLF_LICENSE_TEXT_FSCB, NLF_ULICENSE_TEXT_FSCB));
              if (!p->parms[2])
                p->parms[2] = DefineInnerLangString(NLF_BTN_LICENSE_AGREE);

              license_fscb++;
            }
            else if (p->dlg_id == IDD_LICENSE_FSRB) {
              p->flags |= PF_LICENSE_FORCE_SELECTION;

              if (!p->parms[0])
                p->parms[0] = DefineInnerLangString(LS(NLF_LICENSE_TEXT_FSRB, NLF_ULICENSE_TEXT_FSRB));
              if (!p->parms[2])
                p->parms[2] = DefineInnerLangString(NLF_BTN_LICENSE_AGREE);
              if (!p->parms[3])
                p->parms[3] = DefineInnerLangString(NLF_BTN_LICENSE_DISAGREE);

              license_fsrb++;
            }
            break;
          }
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
          case IDD_SELCOM:
          {
            if (!p->parms[0])
              p->parms[0] = DefineInnerLangString(LS(NLF_COMP_TEXT, NLF_UCOMP_TEXT));
            if (!p->parms[1])
              p->parms[1] = DefineInnerLangString(LS(NLF_COMP_SUBTEXT1, NLF_UCOMP_SUBTEXT1));
            if (!p->parms[2])
              p->parms[2] = DefineInnerLangString(LS(NLF_COMP_SUBTEXT2, NLF_UCOMP_SUBTEXT2));
            if (!p->parms[3] && !uninstall_mode && HasUserDefined(NLF_COMP_SUBTEXT1))
              p->parms[3] = p->parms[1];
            if (!p->parms[4] && !uninstall_mode && HasUserDefined(NLF_COMP_SUBTEXT2))
              p->parms[4] = p->parms[2];
            else if (!p->parms[4])
              p->parms[4] = DefineInnerLangString(LS(NLF_COMP_SUBTEXT1_NO_INST_TYPES, NLF_UCOMP_SUBTEXT1_NO_INST_TYPES));

            DefineInnerLangString(NLF_SPACE_REQ);
            DefineInnerLangString(NLF_BYTE);
            DefineInnerLangString(NLF_KILO);
            DefineInnerLangString(NLF_MEGA);
            DefineInnerLangString(NLF_GIGA);

            selcom++;
            break;
          }
#endif
          case IDD_DIR:
          {
            if (!p->parms[0])
              p->parms[0] = DefineInnerLangString(LS(NLF_DIR_TEXT, NLF_UDIR_TEXT));
            if (!p->parms[1])
              p->parms[1] = DefineInnerLangString(LS(NLF_DIR_SUBTEXT, NLF_UDIR_SUBTEXT));
            if (!p->parms[2])
              p->parms[2] = DefineInnerLangString(NLF_BTN_BROWSE);
            if (!p->parms[3])
              p->parms[3] = DefineInnerLangString(LS(NLF_DIR_BROWSETEXT, NLF_UDIR_BROWSETEXT));
            if (!p->parms[4])
              p->parms[4] = m_UserVarNames.get("INSTDIR");
            else
              p->parms[4]--;

            DefineInnerLangString(NLF_SPACE_AVAIL);
            DefineInnerLangString(NLF_SPACE_REQ);
            DefineInnerLangString(NLF_BYTE);
            DefineInnerLangString(NLF_KILO);
            DefineInnerLangString(NLF_MEGA);
            DefineInnerLangString(NLF_GIGA);
#ifdef NSIS_CONFIG_LOG
            DefineInnerLangString(NLF_LOG_INSTALL_PROCESS);
#endif

            dir++;
            break;
          }
          case IDD_INSTFILES:
          {
            if (!p->parms[1])
              p->parms[1] = DefineInnerLangString(NLF_BTN_DETAILS);
            if (!p->parms[2])
              p->parms[2] = DefineInnerLangString(NLF_COMPLETED);

            DefineInnerLangString(NLF_COPY_DETAILS);

            instlog++;
            instlog_used++;
            break;
          }
          case IDD_UNINST:
          {
            if (!p->parms[0])
              p->parms[0] = DefineInnerLangString(NLF_UNINST_TEXT);
            if (!p->parms[1])
              p->parms[1] = DefineInnerLangString(NLF_UNINST_SUBTEXT);
            if (!p->parms[4])
              p->parms[4] = m_UserVarNames.get("INSTDIR");
            else
              p->parms[4]--;

            uninstconfirm++;
            break;
          }
        }

        p->flags &= ~PF_PAGE_EX;
      }

      p--;

      if (!p->next)
        p->next = DefineInnerLangString(NLF_BTN_CLOSE);
      if (p->wndproc_id == PWP_COMPLETED)
        (p-1)->next = DefineInnerLangString(NLF_BTN_CLOSE);

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (uninstall_mode) {
        if (!uenable_last_page_cancel && instlog_used)
          p->flags &= ~PF_CANCEL_ENABLE;
      }
      else
#endif
      {
        if (!enable_last_page_cancel && instlog_used)
          p->flags &= ~PF_CANCEL_ENABLE;
      }

      if (!instlog_used) {
        warning("%sage instfiles not used, no sections will be executed!", uninstall_mode ? "Uninstall p" : "P");
      }
    }
  }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (!uninstall_mode) {
      set_uninstall_mode(1);
      goto again;
    }
    else
      set_uninstall_mode(0);
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT


  SCRIPT_MSG("Done!\n");

#define REMOVE_ICON(id) if (disable_window_icon) { \
    BYTE* dlg = res_editor->GetResource(RT_DIALOG, MAKEINTRESOURCE(id), NSIS_DEFAULT_LANG); \
    if (dlg) { \
      CDialogTemplate dt(dlg,uDefCodePage); \
      free(dlg); \
      if (dt.RemoveItem(IDC_ULICON)) { \
        DialogItemTemplate* text = dt.GetItem(IDC_INTROTEXT); \
        DialogItemTemplate* prog = dt.GetItem(IDC_PROGRESS); \
        if (text) { \
          text->sWidth += text->sX; \
          text->sX = 0; \
        } \
        if (prog) { \
          prog->sWidth += prog->sX; \
          prog->sX = 0; \
        } \
         \
        DWORD dwSize; \
        dlg = dt.Save(dwSize); \
        res_editor->UpdateResource(RT_DIALOG, MAKEINTRESOURCE(id), NSIS_DEFAULT_LANG, dlg, dwSize); \
      } \
      res_editor->FreeResource(dlg); \
    } \
  }

  try {
    SCRIPT_MSG("Removing unused resources... ");
    init_res_editor();
#ifdef NSIS_CONFIG_LICENSEPAGE
    if (!license_normal) {
      res_editor->UpdateResource(RT_DIALOG, IDD_LICENSE, NSIS_DEFAULT_LANG, 0, 0);
    }
    else REMOVE_ICON(IDD_LICENSE);
    if (!license_fsrb) {
      res_editor->UpdateResource(RT_DIALOG, IDD_LICENSE_FSRB, NSIS_DEFAULT_LANG, 0, 0);
    }
    else REMOVE_ICON(IDD_LICENSE_FSRB);
    if (!license_fscb) {
      res_editor->UpdateResource(RT_DIALOG, IDD_LICENSE_FSCB, NSIS_DEFAULT_LANG, 0, 0);
    }
    else REMOVE_ICON(IDD_LICENSE_FSCB);
#endif // NSIS_CONFIG_LICENSEPAGE
#ifdef NSIS_CONFIG_COMPONENTPAGE
    if (!selcom) {
      res_editor->UpdateResource(RT_DIALOG, IDD_SELCOM, NSIS_DEFAULT_LANG, 0, 0);
      res_editor->UpdateResource(RT_BITMAP, IDB_BITMAP1, NSIS_DEFAULT_LANG, 0, 0);
    }
    else REMOVE_ICON(IDD_SELCOM);
#endif // NSIS_CONFIG_COMPONENTPAGE
    if (!dir) {
      res_editor->UpdateResource(RT_DIALOG, IDD_DIR, NSIS_DEFAULT_LANG, 0, 0);
    }
    else REMOVE_ICON(IDD_DIR);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (!uninstconfirm) {
      res_editor->UpdateResource(RT_DIALOG, IDD_UNINST, NSIS_DEFAULT_LANG, 0, 0);
    }
    else REMOVE_ICON(IDD_UNINST);
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT
    if (!instlog) {
      res_editor->UpdateResource(RT_DIALOG, IDD_INSTFILES, NSIS_DEFAULT_LANG, 0, 0);
    }
    else REMOVE_ICON(IDD_INSTFILES);
    if (!main) {
      res_editor->UpdateResource(RT_DIALOG, IDD_INST, NSIS_DEFAULT_LANG, 0, 0);
      if (!build_compress_whole && !build_crcchk)
        res_editor->UpdateResource(RT_DIALOG, IDD_VERIFY, NSIS_DEFAULT_LANG, 0, 0);
    }

    SCRIPT_MSG("Done!\n");
  }
  catch (exception& err) {
    ERROR_MSG("\nError: %s\n", err.what());
    return PS_ERROR;
  }

  return PS_OK;
}
#endif // NSIS_CONFIG_VISIBLE_SUPPORT

#ifdef NSIS_CONFIG_COMPONENTPAGE
void CEXEBuild::PrepareInstTypes()
{
  if (!(cur_header->flags & CH_FLAGS_NO_CUSTOM))
    cur_header->install_types[NSIS_MAX_INST_TYPES] = DefineInnerLangString(NLF_COMP_CUSTOM);

  // set insttype list for RO sections that didn't use SectionIn
  int i = cur_header->blocks[NB_SECTIONS].num;
  section *sections = (section *) cur_sections->get();

  while (i--)
  {
    if (sections[i].flags & SF_RO && !sections[i].install_types)
      sections[i].install_types = ~0;
  }

  // set selection to first insttype
  if (cur_header->install_types[0])
  {
    int i = cur_header->blocks[NB_SECTIONS].num;
    section *sections = (section *) cur_sections->get();

    // if /o was used abort since the user did his manual choice
    while (i--)
      if ((sections[i].flags & SF_SELECTED) == 0)
        return;

    i = cur_header->blocks[NB_SECTIONS].num;

    while (i--)
      if ((sections[i].install_types & 1) == 0)
        sections[i].flags &= ~SF_SELECTED;
  }
}
#endif

void CEXEBuild::PrepareHeaders(IGrowBuf *hdrbuf)
{
  hdrbuf->add(cur_header,sizeof(header));
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  cur_header->blocks[NB_PAGES].offset = hdrbuf->getlen();
  hdrbuf->add(cur_pages->get(),cur_pages->getlen());
#endif
  cur_header->blocks[NB_SECTIONS].offset = hdrbuf->getlen();
  hdrbuf->add(cur_sections->get(),cur_sections->getlen());
  cur_header->blocks[NB_ENTRIES].offset = hdrbuf->getlen();
  hdrbuf->add(cur_entries->get(),cur_entries->getlen());
  cur_header->blocks[NB_STRINGS].offset = hdrbuf->getlen();
  hdrbuf->add(cur_strlist->get(),cur_strlist->getlen());
  cur_header->blocks[NB_LANGTABLES].offset = hdrbuf->getlen();
  hdrbuf->add(cur_langtables->get(),cur_langtables->getlen());
  cur_header->blocks[NB_CTLCOLORS].offset = hdrbuf->getlen();
  hdrbuf->add(cur_ctlcolors->get(),cur_ctlcolors->getlen());
#ifdef NSIS_SUPPORT_BGBG
  if (cur_header->bg_color1 != -1)
  {
    bg_font.lfFaceName[LF_FACESIZE-1]=0;
    cur_header->blocks[NB_BGFONT].offset = hdrbuf->getlen();
    hdrbuf->add(&bg_font,sizeof(LOGFONT));
  }
#endif

  memcpy(hdrbuf->get(),cur_header,sizeof(header));
}

int CEXEBuild::check_write_output_errors() const
{
  if (has_called_write_output)
  {
    ERROR_MSG("Error (write_output): write_output already called, can't continue\n");
    return PS_ERROR;
  }

  if (!build_output_filename[0])
  {
    ERROR_MSG("Error: invalid script: never had OutFile command\n");
    return PS_ERROR;
  }

  if (!build_sections.getlen())
  {
    ERROR_MSG("Error: invalid script: no sections specified\n");
    return PS_ERROR;
  }

  if (!build_entries.getlen())
  {
    ERROR_MSG("Error: invalid script: no entries specified\n");
    return PS_ERROR;
  }

  if (build_cursection)
  {
    ERROR_MSG("Error: Section left open at EOF\n");
    return PS_ERROR;
  }

  if (subsection_open_cnt)
  {
    ERROR_MSG("Error: SubSection left open at EOF\n");
    return PS_ERROR;
  }

  if (cur_page)
  {
    ERROR_MSG("Error: PageEx still open at EOF, cannot proceed\n");
    return PS_ERROR;
  }

  // deal with functions, for both install and uninstall modes.
  if (build_cursection_isfunc)
  {
    ERROR_MSG("Error: Function still open at EOF, cannot proceed\n");
    return PS_ERROR;
  }

  return PS_OK;
}

int CEXEBuild::prepare_uninstaller() {
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (ubuild_entries.getlen())
  {
    if (!uninstaller_writes_used)
    {
      warning("Uninstall section found but WriteUninstaller never used - no uninstaller will be created.");
      return PS_OK;
    }

    build_uninst.flags|=build_header.flags&(CH_FLAGS_PROGRESS_COLORED|CH_FLAGS_NO_ROOT_DIR);

    set_uninstall_mode(1);
    DefineInnerLangString(NLF_UCAPTION);
    if (resolve_coderefs("uninstall"))
      return PS_ERROR;
#ifdef NSIS_CONFIG_COMPONENTPAGE 
    // set sections to the first insttype
    PrepareInstTypes();
#endif
    set_uninstall_mode(0);
  }
  else if (uninstaller_writes_used)
  {
    ERROR_MSG("Error: no Uninstall section specified, but WriteUninstaller used %d time(s)\n",uninstaller_writes_used);
    return PS_ERROR;
  }
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT
  return PS_OK;
}

int CEXEBuild::pack_exe_header()
{
  if (!(build_packname[0] && build_packcmd[0])) {
    // header not asked to be packed
    return PS_OK;
  }

  // write out exe header, pack, read back in, align to 512, and
  // update the header info
  FILE *tmpfile=FOPEN(build_packname,"wb");
  if (!tmpfile)
  {
    ERROR_MSG("Error: writing temporary file \"%s\" for pack\n",build_packname);
    return PS_ERROR;
  }
  fwrite(header_data_new,1,exeheader_size_new,tmpfile);
  fclose(tmpfile);
  if (system(build_packcmd) == -1)
  {
    remove(build_packname);
    ERROR_MSG("Error: calling packer on \"%s\"\n",build_packname);
    return PS_ERROR;
  }
  tmpfile=FOPEN(build_packname,"rb");
  if (!tmpfile)
  {
    remove(build_packname);
    ERROR_MSG("Error: reading temporary file \"%s\" after pack\n",build_packname);
    return PS_ERROR;
  }
  fseek(tmpfile,0,SEEK_END);
  exeheader_size_new=ftell(tmpfile);
  fseek(tmpfile,0,SEEK_SET);
  unsigned char *header_data_older=header_data_new;
  header_data_new=(unsigned char *)malloc(exeheader_size_new);
  if (!header_data_new)
  {
    free(header_data_older);
    fclose(tmpfile);
    ERROR_MSG("Error: malloc(%d) failed (exepack)\n",exeheader_size_new);
    return PS_ERROR;
  }
  memset(header_data_new,0,exeheader_size_new);
  fread(header_data_new,1,exeheader_size_new,tmpfile);
  fclose(tmpfile);
  remove(build_packname);

  return PS_OK;
}

int CEXEBuild::write_output(void)
{
#ifndef NSIS_CONFIG_CRC_SUPPORT
  build_crcchk=0;
#endif

  RET_UNLESS_OK( check_write_output_errors() );

  has_called_write_output=true;

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  RET_UNLESS_OK( add_plugins_dir_initializer() );
#endif //NSIS_CONFIG_PLUGIN_SUPPORT

#ifdef NSIS_SUPPORT_VERSION_INFO
  RET_UNLESS_OK( AddVersionInfo() );
#endif //NSIS_SUPPORT_VERSION_INFO

  RET_UNLESS_OK( prepare_uninstaller() );

  DefineInnerLangString(NLF_CAPTION);
  if (resolve_coderefs("install"))
    return PS_ERROR;

#ifdef NSIS_CONFIG_COMPONENTPAGE
  // set sections to the first insttype
  PrepareInstTypes();
#endif

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  RET_UNLESS_OK( ProcessPages() );
#endif //NSIS_CONFIG_VISIBLE_SUPPORT

  // Generate language tables
  RET_UNLESS_OK( GenerateLangTables() );

  init_res_editor();
  VerifyDeclaredUserVarRefs(&m_UserVarNames);
  int MaxUserVars = m_UserVarNames.getnum();
  // -1 because the default size is 1
  if (!res_editor->AddExtraVirtualSize2PESection(VARS_SECTION_NAME, (MaxUserVars - 1) * sizeof(NSIS_STRING)))
  {
    ERROR_MSG("Internal compiler error #12346: invalid exehead cannot find section \"%s\"!\n", VARS_SECTION_NAME);
    return PS_ERROR;
  }

  // Save all changes to the exe header
  try {
    close_res_editor();
  }
  catch (exception& err) {
    ERROR_MSG("\nError: %s\n", err.what());
    return PS_ERROR;
  }

  RET_UNLESS_OK( pack_exe_header() );


  build_optimize_datablock=0;

  int data_block_size_before_uninst = build_datablock.getlen();

  RET_UNLESS_OK( uninstall_generate() );

  int crc=0;

  {
    string full_path = get_full_path(build_output_filename);
    notify(MAKENSIS_NOTIFY_OUTPUT, full_path.c_str());
    INFO_MSG("\nOutput: \"%s\"\n", full_path.c_str());
  }

  FILE *fp = FOPEN(build_output_filename,"w+b");
  if (!fp)
  {
    ERROR_MSG("Can't open output file\n");
    return PS_ERROR;
  }

  if ((int)fwrite(header_data_new,1,exeheader_size_new,fp) != exeheader_size_new)
  {
    ERROR_MSG("Error: can't write %d bytes to output\n",exeheader_size_new);
    fclose(fp);
    return PS_ERROR;
  }

#ifdef NSIS_CONFIG_CRC_SUPPORT
  #ifdef NSIS_CONFIG_CRC_ANAL
    crc=CRC32(crc,header_data_new,exeheader_size_new);
  #else
    crc=CRC32(crc,header_data_new+512,exeheader_size_new-512);
  #endif
#endif

  int exeheader_size_new_aligned = (exeheader_size_new + 511) & ~511;
  if (exeheader_size_new_aligned != exeheader_size_new) {
    // align to 512
    const unsigned char z = 0;
    int write_size = exeheader_size_new_aligned - exeheader_size_new;
    for (int i=0; i<write_size; i++) {
      if ((int)fwrite(&z,1,1,fp) != 1)
      {
        ERROR_MSG("Error: can't write %d bytes to output\n",write_size);
        fclose(fp);
        return PS_ERROR;
      }
#ifdef NSIS_CONFIG_CRC_SUPPORT
      crc=CRC32(crc,&z,1);
#endif
    }
    exeheader_size_new = exeheader_size_new_aligned;
  }

  firstheader fh={0,};
  fh.nsinst[0]=FH_INT1;
  fh.nsinst[1]=FH_INT2;
  fh.nsinst[2]=FH_INT3;

#ifdef NSIS_CONFIG_CRC_SUPPORT
  fh.flags=(build_crcchk?(build_crcchk==2?FH_FLAGS_FORCE_CRC:0):FH_FLAGS_NO_CRC);
#else
  fh.flags=0;
#endif
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  if (build_header.flags&(CH_FLAGS_SILENT|CH_FLAGS_SILENT_LOG)) fh.flags |= FH_FLAGS_SILENT;
#endif
  fh.siginfo=FH_SIG;

  int installinfo_compressed;
  int fd_start = 0;

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  if (build_compress_whole)
  {
    int n;
    if (compressor == &lzma_compressor)
      n = ((CLZMA *) compressor)->Init(build_compress_level, build_compress_dict_size);
    else
      n = compressor->Init(build_compress_level);
    if (n != C_OK)
    {
      ERROR_MSG("Internal compiler error #12345: deflateInit() failed(%d - %s).\n", n, compressor->GetErrStr(n));
      return PS_ERROR;
    }
  }
#endif

  {
    GrowBuf ihd;
    {
      GrowBuf hdrcomp;

      PrepareHeaders(&hdrcomp);

      if (add_data((char*)hdrcomp.get(),hdrcomp.getlen(),&ihd) < 0)
        return PS_ERROR;

      fh.length_of_header=hdrcomp.getlen();
      installinfo_compressed=ihd.getlen();
    }

    if (!build_compress_whole)
      fh.length_of_all_following_data=ihd.getlen()+build_datablock.getlen()+(int)sizeof(firstheader)+(build_crcchk?sizeof(int):0);
    else
      fd_start=ftell(fp);

    if (fwrite(&fh,1,sizeof(fh),fp) != sizeof(fh))
    {
      ERROR_MSG("Error: can't write %d bytes to output\n",sizeof(fh));
      fclose(fp);
      return PS_ERROR;
    }

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    if (build_compress_whole) {
      if (deflateToFile(fp,(char*)ihd.get(),ihd.getlen()))
      {
        fclose(fp);
        return PS_ERROR;
      }
    }
    else 
#endif
    {
      if (fwrite(ihd.get(),1,ihd.getlen(),fp) != (unsigned int)ihd.getlen())
      {
        ERROR_MSG("Error: can't write %d bytes to output\n",ihd.getlen());
        fclose(fp);
        return PS_ERROR;
      }
#ifdef NSIS_CONFIG_CRC_SUPPORT
      crc=CRC32(crc,(unsigned char*)&fh,sizeof(fh));
      crc=CRC32(crc,(unsigned char*)ihd.get(),ihd.getlen());
#endif
    }
  }

  INFO_MSG("Install: ");
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  int np=build_header.blocks[NB_PAGES].num;
  INFO_MSG("%d page%s (%d bytes), ",np,np==1?"":"s",np*sizeof(page));
#endif
  {
    int ns=build_sections.getlen()/sizeof(section);
    section *s=(section*)build_sections.get();
    int x;
    int req=0;
    for (x = 1; x < ns; x ++)
    {
      if (!s[x].name_ptr || s[x].flags & SF_RO) req++;
    }
    INFO_MSG("%d section%s",ns,ns==1?"":"s");
    if (req)
    {
      INFO_MSG(" (%d required)",req);
    }
    INFO_MSG(" (%d bytes), ", build_sections.getlen());
  }
  int ne=build_header.blocks[NB_ENTRIES].num;
  INFO_MSG("%d instruction%s (%d bytes), ",ne,ne==1?"":"s",ne*sizeof(entry));
  int ns=build_strlist.getnum();
  INFO_MSG("%d string%s (%d bytes), ",ns,ns==1?"":"s",build_strlist.getlen());
  int nlt=build_header.blocks[NB_LANGTABLES].num;
  INFO_MSG("%d language table%s (%d bytes).\n",nlt,nlt==1?"":"s",build_langtables.getlen());
  if (ubuild_entries.getlen())
  {
    INFO_MSG("Uninstall: ");
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    np=build_uninst.blocks[NB_PAGES].num;
    INFO_MSG("%d page%s (%d bytes), \n",np,np==1?"":"s",ubuild_pages.getlen());
#endif
    {
      int ns=ubuild_sections.getlen()/sizeof(section);
      section *s=(section*)ubuild_sections.get();
      int x;
      int req=0;
      for (x = 1; x < ns; x ++)
      {
        if (!s[x].name_ptr || s[x].flags & SF_RO) req++;
      }
      INFO_MSG("%d section%s",ns,ns==1?"":"s");
      if (req)
      {
        INFO_MSG(" (%d required)",req);
      }
      INFO_MSG(" (%d bytes), ", ubuild_sections.getlen());
    }
    ne=build_uninst.blocks[NB_ENTRIES].num;
    INFO_MSG("%d instruction%s (%d bytes), ",ne,ne==1?"":"s",ubuild_entries.getlen());
    ns=ubuild_strlist.getnum();
    INFO_MSG("%d string%s (%d bytes), ",ns,ns==1?"":"s",ubuild_strlist.getlen());
    nlt=build_uninst.blocks[NB_LANGTABLES].num;
    INFO_MSG("%d language table%s (%d bytes).\n",nlt,nlt==1?"":"s",ubuild_langtables.getlen());
  }


  if (db_opt_save)
  {
    int total_out_size_estimate=
      exeheader_size_new+sizeof(fh)+build_datablock.getlen()+(build_crcchk?sizeof(int):0);
#ifdef _WIN32
    int pc=MulDiv(db_opt_save,1000,db_opt_save+total_out_size_estimate);
#else
    int pc=(int)(((long long)db_opt_save*1000)/(db_opt_save+total_out_size_estimate));
#endif
    INFO_MSG("Datablock optimizer saved %d bytes (~%d.%d%%).\n",db_opt_save,
      pc/10,pc%10);
  }

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  INFO_MSG("\nUsing %s%s compression.\n\n", compressor->GetName(), build_compress_whole?" (compress whole)":"");
#endif

  int total_usize=exeheader_size;

  INFO_MSG("EXE header size:          %10d / %d bytes\n",exeheader_size_new,exeheader_size);

  if (build_compress_whole) {
    INFO_MSG("Install code:                          (%d bytes)\n",
      sizeof(fh)+fh.length_of_header);
  }
  else {
    INFO_MSG("Install code:             %10d / %d bytes\n",
      sizeof(fh)+installinfo_compressed,
      sizeof(fh)+fh.length_of_header);
  }

  total_usize+=sizeof(fh)+fh.length_of_header;

  {
    int dbsize, dbsizeu;
    dbsize = build_datablock.getlen();
    if (uninstall_size>0) dbsize-=uninstall_size;

    if (build_compress_whole) {
      dbsizeu=dbsize;
      INFO_MSG("Install data:                          (%d bytes)\n",dbsizeu);
    }
    else {
      dbsizeu = db_full_size - uninstall_size_full;
      INFO_MSG("Install data:             %10d / %d bytes\n",dbsize,dbsizeu);
    }
    total_usize+=dbsizeu;
  }

  if (uninstall_size>=0)
  {
    if (build_compress_whole)
      INFO_MSG("Uninstall code+data:                   (%d bytes)\n",uninstall_size_full);
    else
      INFO_MSG("Uninstall code+data:          %6d / %d bytes\n",uninstall_size,uninstall_size_full);
    total_usize+=uninstall_size_full;
  }

  if (build_compress_whole) {
    INFO_MSG("Compressed data:          ");
  }

  if (build_datablock.getlen())
  {
    build_datablock.setro(TRUE);
    int dbl = build_datablock.getlen();
    int left = dbl;
    while (left > 0)
    {
      int l = min(build_filebuflen, left);
      char *dbptr = (char *) build_datablock.get(dbl - left, l);
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
      if (build_compress_whole)
      {
        if (deflateToFile(fp,dbptr,l))
        {
          fclose(fp);
          return PS_ERROR;
        }
      }
      else 
#endif
      {
#ifdef NSIS_CONFIG_CRC_SUPPORT
        crc=CRC32(crc,(unsigned char *)dbptr,l);
#endif
        if ((int)fwrite(dbptr,1,l,fp) != l)
        {
          ERROR_MSG("Error: can't write %d bytes to output\n",l);
          fclose(fp);
          return PS_ERROR;
        }
        fflush(fp);
      }
      build_datablock.release();
      left -= l;
    }
    build_datablock.setro(FALSE);
    build_datablock.clear();
  }
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  if (build_compress_whole) 
  {
    if (deflateToFile(fp,NULL,0))
    {
      fclose(fp);
      return PS_ERROR;
    }
    compressor->End();

    unsigned fend = ftell(fp);

    fh.length_of_all_following_data=ftell(fp)-fd_start+(build_crcchk?sizeof(int):0);
    INFO_MSG(
      "%10d / %d bytes\n",
      ftell(fp) - fd_start,
      data_block_size_before_uninst + fh.length_of_header + sizeof(firstheader) + uninstall_size_full
    );

    fseek(fp,fd_start,SEEK_SET);
    fwrite(&fh,sizeof(fh),1,fp);

#ifdef NSIS_CONFIG_CRC_SUPPORT
    if (build_crcchk)
    {
      // check rest of CRC
      fseek(fp,fd_start,SEEK_SET);
      for (;;)
      {
        char buf[32768];
        int l=fread(buf,1,sizeof(buf),fp);
        if (!l) break;
        crc=CRC32(crc,(unsigned char *)buf,l);
      }
    }
#endif
    fseek(fp,fend,SEEK_SET); // reset eof flag
  }
#endif

  if (build_crcchk)
  {
    total_usize+=sizeof(int);
    if (fwrite(&crc,1,sizeof(int),fp) != sizeof(int))
    {
      ERROR_MSG("Error: can't write %d bytes to output\n",sizeof(int));
      fclose(fp);
      return PS_ERROR;
    }
    INFO_MSG("CRC (0x%08X):                  4 / 4 bytes\n",crc);
  }
  INFO_MSG("\n");
  {
#ifdef _WIN32
    int pc=MulDiv(ftell(fp),1000,total_usize);
#else
    int pc=(int)(((long long)ftell(fp)*1000)/(total_usize));
#endif
    INFO_MSG("Total size:               %10d / %d bytes (%d.%d%%)\n",
      ftell(fp),total_usize,pc/10,pc%10);
  }
  fclose(fp);
  print_warnings();
  return PS_OK;
}

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
int CEXEBuild::deflateToFile(FILE *fp, char *buf, int len) // len==0 to flush
{
  build_compressor_set=true;

  char obuf[65536];
  int flush=0;
  compressor->SetNextIn(buf,len);
  if (!buf||!len)
  {
    char a;
    compressor->SetNextIn(&a,0);
    flush=C_FINISH;
  }
  for (;;)
  {
    compressor->SetNextOut(obuf,sizeof(obuf));
    int ret=compressor->Compress(flush);
    if (ret<0 && (ret!=-1 || !flush))
    {
      ERROR_MSG("Error: deflateToFile: deflate() failed(%d - %s)\n", ret, compressor->GetErrStr(ret));
      return 1;
    }
    int l=compressor->GetNextOut()-obuf;
    if (l)
    {
      if (fwrite(obuf,1,l,fp) != (unsigned)l)
      {
        ERROR_MSG("Error: deflateToFile fwrite(%d) failed\n",l);
        return 1;
      }
      fflush(fp);
    }
    if (!compressor->GetAvailIn() && (!flush || !l)) break;
  }
  return 0;
}
#endif

int CEXEBuild::uninstall_generate()
{
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (ubuild_entries.getlen() && uninstaller_writes_used)
  {
    SCRIPT_MSG("Generating uninstaller... ");

    firstheader fh={0,};

    GrowBuf uhd;
    {
      GrowBuf udata;

      set_uninstall_mode(1);

      PrepareHeaders(&udata);

      fh.length_of_header=udata.getlen();
      int err=add_data((char*)udata.get(),udata.getlen(),&uhd);
      set_uninstall_mode(0);
      if (err < 0) return PS_ERROR;
    }

    int crc=0;

    // Get offsets of icons to replace for uninstall
    // Also makes sure that the icons are there and in the right size.
    icon_offset = generate_unicons_offsets(header_data_new, m_unicon_data);
    if (icon_offset == 0)
      return PS_ERROR;

    entry *ent = (entry *) build_entries.get();
    if (!ent)
      return PS_ERROR;
    int ents = build_header.blocks[NB_ENTRIES].num;
    int uns = uninstaller_writes_used;
    int uninstdata_offset = build_datablock.getlen();
    while (ents--)
    {
      if (ent->which == EW_WRITEUNINSTALLER)
      {
        ent->offsets[1] = uninstdata_offset;
        ent->offsets[2] = unicondata_size;
        uns--;
        if (!uns)
          break;
      }
      ent++;
    }

    if (add_db_data((char *)m_unicon_data,unicondata_size) < 0)
      return PS_ERROR;
#ifdef NSIS_CONFIG_CRC_SUPPORT
    {
      // "create" the uninstaller
      LPBYTE uninst_header = (LPBYTE) malloc(exeheader_size_new);
      if (!uninst_header)
        return PS_ERROR;

      memcpy(uninst_header, header_data_new, exeheader_size_new);

      // patch the icons
      LPBYTE seeker = m_unicon_data;
      while (*seeker) {
        DWORD dwSize = *(LPDWORD) seeker;
        seeker += sizeof(DWORD);
        DWORD dwOffset = *(LPDWORD) seeker;
        seeker += sizeof(DWORD);
        memcpy(uninst_header + dwOffset, seeker, dwSize);
        seeker += dwSize;
      }

#ifdef NSIS_CONFIG_CRC_ANAL
      crc=CRC32(crc, uninst_header, exeheader_size_new);
#else
      crc=CRC32(crc, uninst_header + 512, exeheader_size_new - 512);
#endif

      free(uninst_header);
    }
#endif
    fh.nsinst[0]=FH_INT1;
    fh.nsinst[1]=FH_INT2;
    fh.nsinst[2]=FH_INT3;
    fh.flags=FH_FLAGS_UNINSTALL;
    fh.flags|=(build_crcchk?(build_crcchk==2?FH_FLAGS_FORCE_CRC:0):FH_FLAGS_NO_CRC);
#ifdef NSIS_CONFIG_SILENT_SUPPORT
    if (build_uninst.flags&(CH_FLAGS_SILENT|CH_FLAGS_SILENT_LOG)) fh.flags |= FH_FLAGS_SILENT;
#endif
    fh.siginfo=FH_SIG;
    fh.length_of_all_following_data=
      uhd.getlen()+ubuild_datablock.getlen()+(int)sizeof(firstheader)+(build_crcchk?sizeof(int):0);

    MMapBuf udata;

    udata.add(&fh, sizeof(fh));

    ubuild_datablock.setro(TRUE);

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    if (build_compress_whole) {
      // compress uninstaller too
      {
        char obuf[65536];
        int n;
        if (compressor == &lzma_compressor)
          n = ((CLZMA *) compressor)->Init(build_compress_level, build_compress_dict_size);
        else
          n = compressor->Init(build_compress_level);
        if (n != C_OK)
        {
          ERROR_MSG("Internal compiler error #12345: deflateInit() failed(%d - %s).\n", n, compressor->GetErrStr(n));
          extern void quit(); quit();
        }

        compressor->SetNextIn((char*)uhd.get(), uhd.getlen());
        while (compressor->GetAvailIn())
        {
          compressor->SetNextOut(obuf, sizeof(obuf));
          compressor->Compress(0);
          if (compressor->GetNextOut() - obuf > 0)
          {
            udata.add(obuf, compressor->GetNextOut() - obuf);
          }
        }

        int avail_in = ubuild_datablock.getlen();
        int in_pos = 0;
        while (avail_in > 0) {
          int l = min(avail_in, build_filebuflen);

          char *p = (char*)ubuild_datablock.get(in_pos, l);
          compressor->SetNextIn(p, l);

          while (compressor->GetAvailIn())
          {
            compressor->SetNextOut(obuf, sizeof(obuf));
            compressor->Compress(0);
            if (compressor->GetNextOut() - obuf > 0)
              udata.add(obuf, compressor->GetNextOut() - obuf);
          }

          ubuild_datablock.release();

          avail_in -= l;
          in_pos += l;
        }

        for (;;)
        {
          compressor->SetNextOut(obuf, sizeof(obuf));
          compressor->Compress(C_FINISH);
          if (compressor->GetNextOut() - obuf > 0)
            udata.add(obuf, compressor->GetNextOut() - obuf);
          else break;
        }
        compressor->End();
      }

      firstheader *_fh=(firstheader *)udata.get(0, sizeof(firstheader));
      _fh->length_of_all_following_data=udata.getlen()+(build_crcchk?sizeof(int):0);
      udata.release();
    }
    else
#endif//NSIS_CONFIG_COMPRESSION_SUPPORT
    {
      udata.add(uhd.get(), uhd.getlen());

      int st = udata.getlen();
      int length = ubuild_datablock.getlen();
      int left = length;
      udata.resize(st + left);
      while (left > 0)
      {
        int l = min(build_filebuflen, left);
        void *p = ubuild_datablock.get(length - left, l);
        memcpy(udata.get(st + length - left, l), p, l);
        udata.flush(l);
        udata.release();
        ubuild_datablock.release();
        left -= l;
      }
    }

    ubuild_datablock.clear();

    udata.setro(TRUE);

#ifdef NSIS_CONFIG_CRC_SUPPORT
    if (build_crcchk)
    {
      int pos = 0;
      int left = udata.getlen();
      while (left > 0)
      {
        int l = min(build_filebuflen, left);
        crc = CRC32(crc, (unsigned char *) udata.get(pos, l), l);
        udata.release();
        pos += l;
        left -= l;
      }
      udata.add(&crc, sizeof(crc));
    }
#endif

    if (add_db_data(&udata) < 0)
      return PS_ERROR;

    udata.clear();

    //uninstall_size_full=fh.length_of_all_following_data + sizeof(int) + unicondata_size - 32 + sizeof(int);
    uninstall_size_full=fh.length_of_all_following_data+unicondata_size;

    // compressed size
    uninstall_size=build_datablock.getlen()-uninstdata_offset;

    SCRIPT_MSG("Done!\n");
  }
#endif
  return PS_OK;
}


#define SWAP(x,y,i) { i _ii; _ii=x; x=y; y=_ii; }

void CEXEBuild::set_uninstall_mode(int un)
{
  if (un != uninstall_mode)
  {
    uninstall_mode=un;
    if (un)
    {
      cur_datablock=&ubuild_datablock;
      cur_datablock_cache=&ubuild_datablock_cache;
      cur_entries=&ubuild_entries;
      cur_instruction_entry_map=&ubuild_instruction_entry_map;
      cur_functions=&ubuild_functions;
      cur_labels=&ubuild_labels;
      cur_pages=&ubuild_pages;
      cur_sections=&ubuild_sections;
      cur_header=&build_uninst;
      cur_strlist=&ubuild_strlist;
      cur_langtables=&ubuild_langtables;
      cur_ctlcolors=&ubuild_ctlcolors;
    }
    else
    {
      cur_datablock=&build_datablock;
      cur_datablock_cache=&build_datablock_cache;
      cur_entries=&build_entries;
      cur_instruction_entry_map=&build_instruction_entry_map;
      cur_functions=&build_functions;
      cur_labels=&build_labels;
      cur_pages=&build_pages;
      cur_sections=&build_sections;
      cur_header=&build_header;
      cur_strlist=&build_strlist;
      cur_langtables=&build_langtables;
      cur_ctlcolors=&build_ctlcolors;
    }

    SWAP(db_opt_save_u,db_opt_save,int);
    SWAP(db_comp_save_u,db_comp_save,int);
    SWAP(db_full_size_u,db_full_size,int);
  }
}

extern FILE *g_output;

void CEXEBuild::warning(const char *s, ...)
{
  char buf[NSIS_MAX_STRLEN*10];
  va_list val;
  va_start(val,s);
#ifdef _WIN32
  vsprintf(buf,s,val);
#else
  vsnprintf(buf,NSIS_MAX_STRLEN*10,s,val);
#endif
  va_end(val);
  m_warnings.add(buf,0);
  notify(MAKENSIS_NOTIFY_WARNING,buf);
  if (display_warnings)
  {
    fprintf(g_output,"warning: %s\n",buf);
    fflush(g_output);
  }
}

void CEXEBuild::warning_fl(const char *s, ...)
{
  char buf[NSIS_MAX_STRLEN*10];
  va_list val;
  va_start(val,s);
#ifdef _WIN32
  vsprintf(buf,s,val);
#else
  vsnprintf(buf,NSIS_MAX_STRLEN*10,s,val);
#endif
  va_end(val);
  sprintf(buf+strlen(buf)," (%s:%d)",curfilename,linecnt);
  m_warnings.add(buf,0);
  notify(MAKENSIS_NOTIFY_WARNING,buf);
  if (display_warnings)
  {
    fprintf(g_output,"warning: %s\n",buf);
    fflush(g_output);
  }
}

void CEXEBuild::ERROR_MSG(const char *s, ...) const
{
#ifdef _WIN32
  if (display_errors || notify_hwnd)
#else
  if (display_errors)
#endif
  {
    char buf[NSIS_MAX_STRLEN*10];
    va_list val;
    va_start(val,s);
#ifdef _WIN32
    vsprintf(buf,s,val);
#else
    vsnprintf(buf,NSIS_MAX_STRLEN*10,s,val);
#endif
    va_end(val);
    notify(MAKENSIS_NOTIFY_ERROR,buf);
    if (display_errors)
    {
      fprintf(g_output,"%s",buf);
      fflush(g_output);
    }
  }
}

void CEXEBuild::SCRIPT_MSG(const char *s, ...) const
{
  if (display_script)
  {
    va_list val;
    va_start(val,s);
    vfprintf(g_output,s,val);
    va_end(val);
    fflush(g_output);
  }
}

void CEXEBuild::INFO_MSG(const char *s, ...) const
{
  if (display_info)
  {
    va_list val;
    va_start(val,s);
    vfprintf(g_output,s,val);
    va_end(val);
    fflush(g_output);
  }
}

void CEXEBuild::print_warnings()
{
  int nw=0,x=m_warnings.getlen();
  if (!x || !display_warnings) return;
  char *p=m_warnings.get();
  while (x>0) if (!p[--x]) nw++;
  fprintf(g_output,"\n%d warning%s:\n",nw,nw==1?"":"s");
  for (x = 0; x < nw; x ++)
  {
    fprintf(g_output,"  %s\n",p);
    p+=strlen(p)+1;
  }
  fflush(g_output);
}

#ifdef _WIN32
void CEXEBuild::notify(notify_e code, const char *data) const
{
  if (notify_hwnd)
  {
    COPYDATASTRUCT cds = {(DWORD)code, strlen(data)+1, (void *) data};
    SendMessage(notify_hwnd, WM_COPYDATA, 0, (LPARAM)&cds);
  }
}
#endif

// Added by Ximon Eighteen 5th August 2002
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
void CEXEBuild::build_plugin_table(void)
{
  if (plugins_processed)
    return;
  plugins_processed=1;

  plugin_used = false;
  uninst_plugin_used = false;
  char* nsisdir = definedlist.find("NSISDIR");
  if (nsisdir)
  {
    char* searchPath = new char [strlen(nsisdir)+9];
    if (searchPath)
    {
      sprintf(searchPath,"%s" PLATFORM_PATH_SEPARATOR_STR "Plugins",nsisdir);
      INFO_MSG("Processing plugin dlls: \"%s" PLATFORM_PATH_SEPARATOR_STR "*.dll\"\n",searchPath);
      m_plugins.FindCommands(searchPath,display_info?true:false);
      INFO_MSG("\n");
      delete[] searchPath;
    }
  }
}

#define FLAG_OFFSET(flag) (FIELD_OFFSET(exec_flags, flag)/sizeof(int))

int CEXEBuild::add_plugins_dir_initializer(void)
{
  if (!plugin_used && !uninst_plugin_used) return PS_OK;

  SCRIPT_MSG("Adding plug-ins initializing function... ");

  bool uninstall = !plugin_used;

  int ret;
  int zero_offset;

  int var_zero;
  var_zero=m_UserVarNames.get("0");

again:
  // Function [un.]Initialize_____Plugins
  ret=add_function(uninstall?"un.Initialize_____Plugins":"Initialize_____Plugins");
  if (ret != PS_OK) return ret;

  // don't move this, depends on [un.]
  zero_offset=add_string("$0");

  // SetDetailsPrint none
  ret=add_entry_direct(EW_UPDATETEXT, 0, 16);
  if (ret != PS_OK) return ret;

  // StrCmp $PLUGINSDIR ""
  ret=add_entry_direct(EW_STRCMP, add_string("$PLUGINSDIR"), 0, 0, ns_label.add("Initialize_____Plugins_done",0));
  if (ret != PS_OK) return ret;
  // Push $0
  ret=add_entry_direct(EW_PUSHPOP, zero_offset);
  if (ret != PS_OK) return ret;
  // ClearErrors
  ret=add_entry_direct(EW_SETFLAG, FLAG_OFFSET(exec_error));
  if (ret != PS_OK) return ret;
  // GetTempFileName $0
  ret=add_entry_direct(EW_GETTEMPFILENAME, var_zero, add_string("$TEMP"));
  if (ret != PS_OK) return ret;
  // Delete $0 - the temp file created
  ret=add_entry_direct(EW_DELETEFILE, zero_offset);
  if (ret != PS_OK) return ret;
  // CraeteDirectory $0 - a dir instead of that temp file
  ret=add_entry_direct(EW_CREATEDIR, zero_offset);
  if (ret != PS_OK) return ret;
  // IfErrors Initialize_____Plugins_error - detect errors
  ret=add_entry_direct(EW_IFFLAG, ns_label.add("Initialize_____Plugins_error",0), 0, FLAG_OFFSET(exec_error));
  if (ret != PS_OK) return ret;
  // Copy $0 to $PLUGINSDIR
  ret=add_entry_direct(EW_ASSIGNVAR, m_UserVarNames.get("PLUGINSDIR"), zero_offset);
  if (ret != PS_OK) return ret;
  // Pop $0
  ret=add_entry_direct(EW_PUSHPOP, var_zero, 1);
  if (ret != PS_OK) return ret;

  // done
  if (add_label("Initialize_____Plugins_done")) return PS_ERROR;
  // Return
  ret=add_entry_direct(EW_RET);
  if (ret != PS_OK) return ret;

  // error
  if (add_label("Initialize_____Plugins_error")) return PS_ERROR;
  // error message box
  ret=add_entry_direct(EW_MESSAGEBOX, MB_OK|MB_ICONSTOP|(IDOK<<20), add_string("Error! Can't initialize plug-ins directory. Please try again later."));
  if (ret != PS_OK) return ret;
  // Quit
  ret=add_entry_direct(EW_QUIT);
  if (ret != PS_OK) return ret;

  // FunctionEnd
  ret=function_end();
  if (ret != PS_OK) return ret;

  if (uninst_plugin_used && !uninstall) {
    uninstall = true;
    goto again;
  }

  SCRIPT_MSG("Done!\n");

  return PS_OK;
}
#endif // NSIS_CONFIG_PLUGIN_SUPPORT

void CEXEBuild::init_res_editor()
{
  build_compressor_set = true;
  if (!res_editor)
    res_editor = new CResourceEditor(header_data_new, exeheader_size_new);
}

void CEXEBuild::close_res_editor()
{
  if (!res_editor) return;
  DWORD newsize;
  // query size
  newsize = res_editor->Save(NULL, newsize);
  unsigned char *new_header = (unsigned char *) malloc(newsize);
  // save
  res_editor->Save(new_header, newsize);
  free(header_data_new);
  header_data_new = new_header;
  exeheader_size_new = (int) newsize;
  delete res_editor;
  res_editor=0;
}

int CEXEBuild::DeclaredUserVar(const char *szVarName)
{
  if (m_ShellConstants.get((char*)szVarName) >= 0)
  {
    ERROR_MSG("Error: name \"%s\" in use by constant\n", szVarName);
    return PS_ERROR;  
  }

  int idxUserVar = m_UserVarNames.get((char*)szVarName);
  if (idxUserVar >= 0)
  {
    ERROR_MSG("Error: variable \"%s\" already declared\n", szVarName);
    return PS_ERROR;  
  }
  const char *pVarName = szVarName;
  int iVarLen = strlen(szVarName);

  if (iVarLen > 60)
  {
    ERROR_MSG("Error: variable name too long!\n");
    return PS_ERROR;
  }
  else if (!iVarLen)
  {
    ERROR_MSG("Error: variable with empty name!\n");
    return PS_ERROR;
  }
  else
  {
    while (*pVarName)
    {
      if (!isSimpleChar(*pVarName))
      {
        ERROR_MSG("Error: invalid charaters in variable name \"%s\", use only charaters [a-z][A-Z][0-9] and '_'\n", szVarName);
        return PS_ERROR;
      }
      pVarName++;
    }
  }

  m_UserVarNames.add(szVarName);
  if (m_UserVarNames.getnum() > MAX_CODED)
  {
    ERROR_MSG("Error: too many user variables declared. Maximum allowed is %u.\n", MAX_CODED - m_iBaseVarsNum);
    return PS_ERROR;
  }
  return PS_OK;
}


int CEXEBuild::GetUserVarIndex(LineParser &line, int token)
{
  char *p = line.gettoken_str(token);
  if ( *p == '$' && *(p+1) )
  {
    int idxUserVar = m_UserVarNames.get((char *)p+1);
    if (idxUserVar >= 0 && m_UserVarNames.get_reference(idxUserVar) >= 0)
    {
      m_UserVarNames.inc_reference(idxUserVar);
      return idxUserVar;
    }
    else
    {
      int idxConst = m_ShellConstants.get((char *)p+1);
      if (idxConst >= 0)
      {
        ERROR_MSG("Error: cannot change constants : %s\n", p);
      }
    }
  }
  return -1;
}

void CEXEBuild::VerifyDeclaredUserVarRefs(UserVarsStringList *pVarsStringList)
{
  for (int i = m_iBaseVarsNum; i < pVarsStringList->getnum(); i++)
  {
    if (!pVarsStringList->get_reference(i))
    {      
      warning("Variable \"%s\" not referenced, wasting memory!", pVarsStringList->idx2name(i));
    }  
  }
}
