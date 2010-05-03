/*
 * build.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2009 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support by Jim Park -- 08/10/2007
 */

#ifndef _BUILD_H_
#define _BUILD_H_

#include "strlist.h"
#include "lineparse.h"
#include "lang.h"
#include "ResourceEditor.h"
#include "ResourceVersionInfo.h"
#include "uservars.h"
#include "ShConstants.h"
#include "mmap.h"
#include "manifest.h"
#include "icon.h"

#include "exehead/fileform.h"
#include "exehead/config.h"

#include "tstring.h"
#include <set>

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
// Added by Sunil Kamath 11 June 2003
#  include <time.h>
#  include <sys/stat.h>
#endif

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
// Changed by Amir Szekely 31st July 2002
#include "compressor.h"
#include "czlib.h"
#include "cbzip2.h"
#include "clzma.h"

#endif//NSIS_CONFIG_COMPRESSION_SUPPORT

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
#  include "Plugins.h"
#endif //NSIS_CONFIG_PLUGIN_SUPPORT

#define PS_OK 0
#define PS_EOF 1
#define PS_ERROR 50
#define PS_WARNING 100

// token placement
#define TP_SEC    1
#define TP_FUNC   2
#define TP_CODE   (TP_SEC | TP_FUNC)
#define TP_GLOBAL 4
#define TP_PAGEEX 8
#define TP_PG     (TP_GLOBAL | TP_PAGEEX)
#define TP_ALL    (TP_CODE | TP_PG)

enum notify_e {
  MAKENSIS_NOTIFY_SCRIPT,
  MAKENSIS_NOTIFY_WARNING,
  MAKENSIS_NOTIFY_ERROR,
  MAKENSIS_NOTIFY_OUTPUT
};

#define PAGE_CUSTOM 0
#define PAGE_LICENSE 1
#define PAGE_COMPONENTS 2
#define PAGE_DIRECTORY 3
#define PAGE_INSTFILES 4
#define PAGE_UNINSTCONFIRM 5
#define PAGE_COMPLETED 6

class CEXEBuild {
  public:
    CEXEBuild();
    void initialize(const TCHAR *makensis_path);
    ~CEXEBuild();

    // to add a warning to the compiler's warning list.
    void warning(const TCHAR *s, ...);
    // warning with file name and line count
    void warning_fl(const TCHAR *s, ...);

    // to add a defined thing.
    void define(const TCHAR *p, const TCHAR *v=TEXT(""));

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    // Added by Ximon Eighteen 5th August 2002
    void build_plugin_table(void);
    int plugins_processed;
#endif //NSIS_CONFIG_PLUGIN_SUPPORT


    // process a script (you can process as many scripts as you want,
    // it is as if they are concatenated)
    int process_script(FILE *fp, const TCHAR *curfilename);
    int process_oneline(TCHAR *line, const TCHAR *curfilename, int lineptr);
    
    // you only get to call write_output once, so use it wisely.
    int write_output(void);

    void print_help(TCHAR *commandname=NULL);

    DefineList definedlist; // List of identifiers marked as "defined" like
                            // C++ macro definitions such as _UNICODE.

    int display_errors;
    int display_script;
    int display_warnings;
    int display_info;

    int linecnt;
    const TCHAR *curfilename;
    FILE *fp;

    HWND notify_hwnd;
    void notify(notify_e code, const TCHAR *data) const;

  private:
    int check_write_output_errors() const;
    int prepare_uninstaller();
    int pack_exe_header();

    int set_compressor(const tstring& compressor, const bool solid);
    int update_exehead(const tstring& file, size_t *size=NULL);
    void update_exehead(const unsigned char *new_exehead, size_t new_size);

    // tokens.cpp
    bool is_valid_token(TCHAR *s);
    int get_commandtoken(TCHAR *s, int *np, int *op, int *pos);

    /**
     * Returns the current "state" by looking at whether it is in a
     * section/function/pagex or global.
     * @return TP_FUNC, TP_SEC, TP_PAGEEX, TP_GLOBAL.
     */
    int GetCurrentTokenPlace();
    int IsTokenPlacedRight(int pos, TCHAR *tok);

    // script.cpp
#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
    TCHAR* set_file_predefine(const TCHAR *);
    void restore_file_predefine(TCHAR *);
    TCHAR* set_timestamp_predefine(const TCHAR *);
    void restore_timestamp_predefine(TCHAR *);
    TCHAR* set_line_predefine(int, BOOL);
    void restore_line_predefine(TCHAR *);
    void set_date_time_predefines();
    void del_date_time_predefines();
#endif
    int parseScript();
    int includeScript(TCHAR *f);
    int MacroExists(const TCHAR *macroname);
    int LoadLicenseFile(TCHAR *file, TCHAR** pdata, LineParser &line);
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
    void ps_addtoline(const TCHAR *str, GrowBuf &linedata, StringList &hist, bool bIgnoreDefines = false);
#else
    void ps_addtoline(const TCHAR *str, GrowBuf &linedata, StringList &hist);
#endif
    int doParse(const TCHAR *str);
    int doCommand(int which_token, LineParser &line);

    int do_add_file(const TCHAR *lgss, int attrib, int recurse, int *total_files, const TCHAR 
      *name_override=0, int generatecode=1, int *data_handle=0, 
      const std::set<tstring>& excluded=std::set<tstring>(), 
      const tstring& basedir=tstring(_T("")), bool dir_created=false);
    int add_file(const tstring& dir, const tstring& file, int attrib, const TCHAR 
      *name_override, int generatecode, int *data_handle);
    int do_add_file_create_dir(const tstring& local_dir, const tstring& dir, int attrib=0);

    GrowBuf m_linebuild; // used for concatenating lines

    // used by doParse to do preprocessing
    struct ifblock
    {
      int hasexeced;
      int elseused;
      int ignore;
      int inherited_ignore;
    } *cur_ifblock;

    TinyGrowBuf build_preprocessor_data;

    void start_ifblock();
    void end_ifblock();
    int num_ifblock();

    int last_line_had_slash;
    bool inside_comment;
    int multiple_entries_instruction;  // 1 (true) or 0 (false)

    void ERROR_MSG(const TCHAR *s, ...) const;
    void SCRIPT_MSG(const TCHAR *s, ...) const;
    void INFO_MSG(const TCHAR *s, ...) const;

    DefineList *searchParseString(const TCHAR *source_string, LineParser *line, int parmOffs, bool ignCase, bool noErrors);

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    int add_plugins_dir_initializer(void);
#endif //NSIS_CONFIG_PLUGIN_SUPPORT

    // build.cpp functions used mostly by script.cpp
    void set_code_type_predefines(const TCHAR *value = NULL);
    int getcurdbsize();
    int add_section(const TCHAR *secname, const TCHAR *defname, int expand=0);
    int section_end();
    int add_function(const TCHAR *funname);
    int function_end();
    void section_add_size_kb(int kb);
    int section_add_flags(int flags);
    int section_add_install_type(int inst_type);
    int add_page(int type);
    int page_end();
    int add_label(const TCHAR *name);
    int add_entry(const entry *ent);
    int add_entry_direct(int which, int o0=0, int o1=0, int o2=0, int o3=0, int o4=0, int o5=0);
    int add_db_data(IMMap *map); // returns offset
    int add_db_data(const char *data, int length); // returns offset
    int add_data(const char *data, int length, IGrowBuf *dblock); // returns offset
    int add_string(const TCHAR *string, int process=1, WORD codepage=CP_ACP); // returns offset (in string table)
    int add_intstring(const int i); // returns offset in stringblock

    int preprocess_string(TCHAR *out, const TCHAR *in, WORD codepage=CP_ACP);

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    // Added by Ximon Eighteen 5th August 2002
    Plugins m_plugins;
    bool plugin_used;
    bool uninst_plugin_used;
    int build_plugin_unload;
#endif //NSIS_CONFIG_PLUGIN_SUPPORT

    // build.cpp functions used mostly within build.cpp
    int datablock_optimize(int start_offset, int first_int);
    void printline(int l);
    int process_jump(LineParser &line, int wt, int *offs);

    int AddVersionInfo();
    int ProcessPages();
    void PrepareInstTypes();
    void AddStandardStrings();
    void PrepareHeaders(IGrowBuf *hdrbuf);
    int SetVarsSection();
    int SetManifest();
    int UpdatePEHeader();

    int resolve_jump_int(const TCHAR *fn, int *a, int offs, int start, int end);
    int resolve_call_int(const TCHAR *fn, const TCHAR *str, int fptr, int *ofs);
    int resolve_instruction(const TCHAR *fn, const TCHAR *str, entry *w, int offs, int start, int end);

    int resolve_coderefs(const TCHAR *str);
    void print_warnings();
    int uninstall_generate();

    /** Are we defining an uninstall version of the code?
     * @param un Use like a boolean to define whether in uninstall mode.
     */
    void set_uninstall_mode(int un);

    // lang.cpp functions and variables
    void InitLangTables();

    /**
     * This function gets a LanguageTable structure for the specified language
     * via LANGID.  If create == true, it will create a new LanguageTable if
     * the appropriate one cannot be found.  If lang is LANG_NEUTRAL (0), then
     * it will get the LanguageTable of the last used language or more
     * correctly, the last Language ID that generated a valid return value
     * (not NULL).
	  *
     * @param lang [in/out] Language ID reference.  If LANG_NEUTRAL, it gets
     * set to thelast used language ID.
     * @param create Create a new LanguageTable?  Default = true.
     * @return Appropriate LanguagTable* if exists, otherwise NULL.
     */
    LanguageTable *GetLangTable(LANGID &lang, bool create = true);

	 /**
	  * Get the language name as a TCHAR* and the code page value via an
	  * out parameter.  It will look for a LanguageTable to get the values.
	  * If not found, then it will set the codepage to English for ANSI
	  * or Unicode for Unicode version of NSIS.  The language name is looked
	  * up via the LanguageTable if it exists, otherwise, it calls
	  * GetLocaleInfo() with the LANGID to get the string.
	  *
	  * This function is not thread-safe!  For a thread-safe version, the
	  * parameter must include the buffer to write to.
	  *
	  * @param lang The language ID
	  * @param codepage [out] The code page referred to by the language ID.
	  * @return The language string in English.
	  */
    const TCHAR *GetLangNameAndCP(LANGID lang, unsigned int *codepage = NULL);

    int DefineLangString(const TCHAR *name, int process=-1);
    int DefineInnerLangString(int id, int process=-1);

    /**
     * A LangString is a string variable that varies in value depending on what
     * language is being used.  This function sets the string value for the
     * variable 'name' for a given language ID.
     * 
     * @return If the language id, the variable name or string is invalid, it will
     * return aPS_ERROR.  If this function call is overwriting a set user string,
     * this will return a PS_WARNING.
     */
    int SetLangString(TCHAR *name, LANGID lang, TCHAR *str);

    /**
     * Sets the user string to the specific NLF_STRINGS id.
     *
     * @return If the id is invalid or the string is not valid, it will return
     * aPS_ERROR.  If this function call is overwriting a set user string,
     * this will return a PS_WARNING.
     */
    int SetInnerString(int id, TCHAR *str);

    int GenerateLangTable(LanguageTable *lt, int num_lang_tables);
    int GenerateLangTables();
    void FillLanguageTable(LanguageTable *table);
    int HasUserDefined(int id) {
      const TCHAR *us = UserInnerStrings.get(id);
      return us && *us;
    };

    LanguageTable *LoadLangFile(TCHAR *filename);
    void DeleteLangTable(LanguageTable *table);

    NLFRef NLFRefs[NLF_STRINGS];
    bool keep_ref;
    StringsArray UserInnerStrings;
    bool defcodepage_set;
    GrowBuf lang_tables;
    LANGID last_used_lang;
    LangStringList build_langstrings;

    int build_langstring_num, ubuild_langstring_num;
    TCHAR build_font[1024];
    int build_font_size;

    unsigned int uDefCodePage;

    // pages stuff
    int license_res_id;
    page *cur_page;  // Current page we are defining, NULL if not.
    int cur_page_type;
    int enable_last_page_cancel, uenable_last_page_cancel;

    int disable_window_icon;

    // User variables stuff
    int GetUserVarIndex(LineParser &line, int token);
    // Added by ramon 3 jun 2003
    UserVarsStringList m_UserVarNames;
    int m_iBaseVarsNum;
    int DeclaredUserVar(const TCHAR *VarName);
    void VerifyDeclaredUserVarRefs(UserVarsStringList *pVarsStringList);

    ConstantsStringList m_ShellConstants;

    // a whole bunch O data.

    tstring stubs_dir;

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    ICompressor *compressor;
    CZlib zlib_compressor;
    CBzip2 bzip2_compressor;
    CLZMA lzma_compressor;
#endif
    bool build_compressor_set;
    bool build_compressor_final;
    bool build_compress_whole;
    int build_compress;
    int build_compress_level;
    int build_compress_dict_size;

    bool no_space_texts;

    bool has_called_write_output;

    TCHAR build_packname[1024], build_packcmd[1024];
    int build_overwrite, build_last_overwrite, build_crcchk,
        build_datesave, build_optimize_datablock,
        build_allowskipfiles; // Added by ramon 23 May 2003

    header build_header, build_uninst, *cur_header;
    int uninstall_mode; // Are we in uinstall mode?  Acts like a bool.
    int uninstall_size,uninstall_size_full;
    int uninstaller_writes_used;

    TCHAR build_output_filename[1024];

    int build_include_depth;

    // Added by ramon 6 jun 2003
#ifdef NSIS_SUPPORT_VERSION_INFO
    CResourceVersionInfo rVersionInfo;
    TCHAR version_product_v[1024];
#endif

    int sectiongroup_open_cnt;
    FastStringList m_warnings;
    GrowBuf m_macros;

    StringList m_macro_entry;

    int db_opt_save, db_comp_save, db_full_size, db_opt_save_u, 
        db_comp_save_u, db_full_size_u;

    FastStringList include_dirs;

    StringList ns_func; // function namespace
    StringList ns_label; // label namespace

    int build_cursection_isfunc; // Are we in the middle of func definition?
    section *build_cursection;   // The section we are defining, NULL if not in section.
                                 // This could be a function or a section.

    // The ubuild prefixed objects / variables are for the uinstall versions
    // of the code.  The cur prefix objects are what the current objects that
    // need to be referenced should be.  What is pointed to by the cur* objects
    // are determined by whether or not we are in uninstall mode or not.
    TinyGrowBuf build_sections, ubuild_sections, *cur_sections;
    GrowBuf build_entries,ubuild_entries, *cur_entries;
    GrowBuf build_instruction_entry_map,ubuild_instruction_entry_map, *cur_instruction_entry_map;
    TinyGrowBuf build_functions, ubuild_functions, *cur_functions;
    TinyGrowBuf build_labels, ubuild_labels, *cur_labels;
    StringList build_strlist, ubuild_strlist, *cur_strlist;
    GrowBuf build_langtables, ubuild_langtables, *cur_langtables;
    TinyGrowBuf build_pages, ubuild_pages, *cur_pages;
    TinyGrowBuf build_ctlcolors, ubuild_ctlcolors, *cur_ctlcolors;

    // don't forget to update the cache after updating the datablock
    // see datablock_optimize for an example
    MMapBuf build_datablock, ubuild_datablock;
    TinyGrowBuf build_datablock_cache, ubuild_datablock_cache;
    IGrowBuf *cur_datablock, *cur_datablock_cache;
    struct cached_db_size
    {
      int first_int; // size | (compressed ? 0x80000000 : 0)
      int start_offset;
    };

    int build_filebuflen;

    TinyGrowBuf verbose_stack;

    unsigned char *m_exehead;
    size_t m_exehead_size;
    size_t m_exehead_original_size;

    bool branding_image_found;
    WORD branding_image_id;

    IconGroup installer_icon;
    IconGroup uninstaller_icon;
    size_t m_unicon_size;

#ifdef NSIS_SUPPORT_BGBG
    LOGFONT bg_font;
    LOGFONT bg_default_font;
#endif

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    int deflateToFile(FILE *fp, char *buf, int len); // len==0 to flush
#endif

    manifest::comctl manifest_comctl;
    manifest::exec_level manifest_exec_level;

    CResourceEditor *res_editor;
    void init_res_editor();
    void close_res_editor();
};

#endif //_BUILD_H_
