#ifndef _BUILD_H_
#define _BUILD_H_

#include <Vector>
using namespace std;

#include "strlist.h"
#include "lineparse.h"
#include "lang.h"

#include "exehead/fileform.h"
#include "exehead/config.h"

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
// Changed by Amir Szekely 31st July 2002
#include "compressor.h"
#include "czlib.h"
#include "cbzip2.h"

#endif//NSIS_CONFIG_COMPRESSION_SUPPORT

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
#include "Plugins.h"
#endif //NSIS_CONFIG_PLUGIN_SUPPORT

#ifdef NSIS_CONFIG_CRC_SUPPORT
extern "C"
{
  unsigned long NSISCALL CRC32(unsigned long crc, const unsigned char *buf, unsigned int len);
};
#endif

#define PS_OK 0
#define PS_EOF 1
#define PS_ENDIF 2
#define PS_ELSE 3
#define PS_ELSE_IF0 4
#define PS_ELSE_IF1 5
#define PS_ERROR 50
#define IS_PS_ELSE(x) (( x ) >= PS_ELSE && ( x ) <= PS_ELSE_IF1)

class CEXEBuild {
  public: 
    CEXEBuild();
    ~CEXEBuild();

    // to add a warning to the compiler's warning list.
    void warning(const char *s, ...);

    // to add a defined thing.
    void define(const char *p, const char *v="");

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    // Added by Ximon Eighteen 5th August 2002
    void build_plugin_table(void);
#endif //NSIS_CONFIG_PLUGIN_SUPPORT


    // process a script (you can process as many scripts as you want,
    // it is as if they are concatenated)
    int process_script(FILE *fp, char *curfilename, int *lineptr);
    int process_oneline(char *line, char *curfilename, int lineptr);
    
    // you only get to call write_output once, so use it wisely.
    int write_output(void);

    void print_help(char *commandname=NULL);

    DefineList definedlist;

    int display_errors;
    int display_script;
    int display_warnings;
    int display_info;

  private:
    // tokens.cpp
    int get_commandtoken(char *s, int *np, int *op);

    // script.cpp
    int parseScript(FILE *fp, const char *curfilename, int *lineptr);
    void ps_addtoline(const char *str, GrowBuf &linedata, StringList &hist);
    int doParse(const char *str, FILE *fp, const char *curfilename, int *lineptr);
    int doCommand(int which_token, LineParser &line, FILE *fp, const char *curfilename, int linecnt);
    int do_add_file(const char *lgss, int attrib, int recurse, int linecnt, int *total_files, const char *name_override=0, int generatecode=1);
    GrowBuf m_linebuild; // used for concatenating lines

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    // Added by Amir Szekely 9th August 2002
    int add_plugins_dir_initializer(void);
#endif //NSIS_CONFIG_PLUGIN_SUPPORT

    void ERROR_MSG(const char *s, ...);
    void SCRIPT_MSG(const char *s, ...);
    void INFO_MSG(const char *s, ...);

    // build.cpp functions used mostly by script.cpp
    int getcurdbsize();
    int add_section(const char *secname, const char *file, int line, const char *defname,  int expand);
    int section_end();
    int add_function(const char *funname);
    int function_end();
    void section_add_size_kb(int kb);
    int section_add_flags(int flags);
    int section_add_install_type(int inst_type);
    int add_label(const char *name);
    int add_entry(const entry *ent);
    int add_data(const char *data, int length, IGrowBuf *dblock=NULL); // returns offset
    int add_string(const char *string); // returns offset (in string table)
    int add_string_main(const char *string, int process=1); // returns offset (in string table)
    int add_string_uninst(const char *string, int process=1); // returns offset (in string table)
    int preprocess_string(char *out, const char *in);

    int make_sure_not_in_secorfunc(const char *str);

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    // Added by Ximon Eighteen 5th August 2002
    Plugins m_plugins;
    bool plugin_used;
    bool uninst_plugin_used;
    int build_plugin_unload;
#endif //NSIS_CONFIG_PLUGIN_SUPPORT

    // build.cpp functions used mostly within build.cpp
    int datablock_optimize(int start_offset);
    void printline(int l);
    int process_jump(LineParser &line, int wt, int *offs);

    int resolve_jump_int(const char *fn, int *a, int offs, int start, int end);
    int resolve_call_int(const char *fn, const char *str, int fptr, int *ofs);
    int resolve_instruction(const char *fn, const char *str, entry *w, int offs, int start, int end);

    int resolve_coderefs(const char *str);
    void print_warnings();
    int uninstall_generate();
    void set_uninstall_mode(int un);

    // lang.cpp by Amir Szekely 3rd August 2002
    StringTable *GetTable(LANGID &lang);
    int SetString(char *string, int id, int process, WORD lang=0);
    int SetString(char *string, int id, int process, StringTable *table);
    int SetUserString(char *name, LANGID lang, char *string, int process=1);
    int WriteStringTables();
    void FillDefaultsIfNeeded(StringTable *table, NLF *nlf=0);
    #define IsNotSet(s) _IsNotSet(string_tables.size()?&(string_tables[0]->s):0)
    bool _IsNotSet(int *str); // Checks if a string is not set in all of the string tables
    #define IsSet(s,lang) _IsSet(string_tables.size()?&(string_tables[0]->s):0,lang)
    bool _IsSet(int *str, WORD lang); // Checks if a string is set in a given string table

    // a whole bunch O data.

    // Added by Amir Szekely 31st July 2002
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    ICompressor *compressor;
    CZlib zlib_compressor;
    CBzip2 bzip2_compressor;
#endif
    bool build_compressor_set;
    bool build_compress_whole;

    // Added by Amir Szekely 2nd August 2002
    vector<NLF*> build_nlfs;
    vector<StringTable*> string_tables;
    LANGID last_used_lang;

    bool no_space_texts;

    int has_called_write_output;

    char build_packname[1024], build_packcmd[1024];
    int build_overwrite, build_compress, build_crcchk, 
        build_datesave, build_optimize_datablock;

    header build_header;
    int uninstall_mode;
    uninstall_header build_uninst;
    int uninstall_size,uninstall_size_full;
    int uninstaller_writes_used;

    char build_output_filename[1024];
    char cur_out_path[1024];

    int subsection_open_cnt;
    StringList m_warnings;
    GrowBuf m_macros;

    StringList m_macro_entry;

    int db_opt_save, db_comp_save, db_full_size, db_opt_save_u, 
        db_comp_save_u, db_full_size_u;
    int build_sections_req,build_sections_div;

    StringList ns_func, ns_label; // function and label namespaces

    int build_cursection_isfunc;
    section *build_cursection;
    GrowBuf build_sections;
    GrowBuf build_entries,ubuild_entries, *cur_entries;
    GrowBuf build_functions, ubuild_functions, *cur_functions;
    GrowBuf build_labels, ubuild_labels, *cur_labels;
    StringList build_strlist,ubuild_strlist;
    GrowBuf build_langtables, ubuild_langtables;
    StringList build_userlangstrings, ubuild_userlangstrings;

    MMapBuf build_datablock, ubuild_datablock; // use GrowBuf here instead of MMapBuf if you want
    IGrowBuf *cur_datablock; 

    unsigned char *header_data_new;
    int exeheader_size_new;
    int enabled_bitmap_offset;
    int icon_offset;
    int m_inst_fileused;
    int m_uninst_fileused;
    bool branding_image_found; // Added by Amir Szekely 29nd July 2002
    WORD branding_image_id; // Added by Amir Szekely 29nd July 2002
    unsigned char *m_unicon_data;
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    int deflateToFile(FILE *fp, char *buf, int len); // len==0 to flush
#endif
};

#endif //_BUILD_H_
