#include <windows.h>
#include <stdio.h>
#include "exehead/config.h"
#include "exehead/fileform.h"

#include "exedata.h"

#include "build.h"
#include "util.h"

#include "ResourceEditor.h"
#include "exehead/resource.h"

void CEXEBuild::define(const char *p, const char *v) 
{ 
  definedlist.add(p,v); 
}


CEXEBuild::~CEXEBuild()
{
  free(header_data_new);
  free(m_unicon_data);
}

CEXEBuild::CEXEBuild()
{
  display_info=1;
  display_script=1;
  display_errors=1;
  display_warnings=1;

  has_called_write_output=0;

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

  strcpy(cur_out_path,"$INSTDIR");

#ifdef NSIS_CONFIG_LOG
  definedlist.add("NSIS_CONFIG_LOG");
#endif
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  definedlist.add("NSIS_CONFIG_UNINSTALL_SUPPORT");
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
  definedlist.add("NSIS_CONFIG_LICENSEPAGE");
#endif
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  definedlist.add("NSIS_CONFIG_SILENT_SUPPORT");
#endif
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  definedlist.add("NSIS_CONFIG_COMPRESSION_SUPPORT");
#endif
#ifdef NSIS_ZLIP_COMPRESS_WHOLE
  definedlist.add("NSIS_ZLIP_COMPRESS_WHOLE");
#endif
#ifdef NSIS_BZIP2_COMPRESS_WHOLE
  definedlist.add("NSIS_BZIP2_COMPRESS_WHOLE");
#endif
  {
    char b[123];
    wsprintf(b,"%d",NSIS_COMPRESS_BZIP2_LEVEL);
    definedlist.add("NSIS_COMPRESS_BZIP2_LEVEL",b);
  }
#ifdef NSIS_CONFIG_CRC_SUPPORT
  definedlist.add("NSIS_CONFIG_CRC_SUPPORT");
#endif
#ifdef NSIS_CONFIG_CRC_ANAL
  definedlist.add("NSIS_CONFIG_CRC_ANAL");
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
  definedlist.add("NSIS_CONFIG_COMPONENTPAGE");
#endif
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  definedlist.add("NSIS_CONFIG_VISIBLE_SUPPORT");
#endif
#ifdef NSIS_CONFIG_XPSTYLE_SUPPORT
  definedlist.add("NSIS_CONFIG_XPSTYLE_SUPPORT");
#endif
#ifdef NSIS_SUPPORT_BGBG
  definedlist.add("NSIS_SUPPORT_BGBG");
#endif
#ifdef NSIS_SUPPORT_ACTIVEXREG
  definedlist.add("NSIS_SUPPORT_ACTIVEXREG");
#endif
#ifdef NSIS_SUPPORT_INTOPTS
  definedlist.add("NSIS_SUPPORT_INTOPTS");
#endif
#ifdef NSIS_SUPPORT_STROPTS
  definedlist.add("NSIS_SUPPORT_STROPTS");
#endif
#ifdef NSIS_SUPPORT_STACK
  definedlist.add("NSIS_SUPPORT_STACK");
#endif
#ifdef NSIS_SUPPORT_FILEFUNCTIONS
  definedlist.add("NSIS_SUPPORT_FILEFUNCTIONS");
#endif
#ifdef NSIS_SUPPORT_FINDFIRST
  definedlist.add("NSIS_SUPPORT_FINDFIRST");
#endif
#ifdef NSIS_SUPPORT_CREATESHORTCUT
  definedlist.add("NSIS_SUPPORT_CREATESHORTCUT");
#endif
#ifdef NSIS_SUPPORT_INIFILES
  definedlist.add("NSIS_SUPPORT_INIFILES");
#endif
#ifdef NSIS_SUPPORT_REGISTRYFUNCTIONS
  definedlist.add("NSIS_SUPPORT_REGISTRYFUNCTIONS");
#endif
#ifdef NSIS_SUPPORT_COPYFILES
  definedlist.add("NSIS_SUPPORT_COPYFILES");
#endif
#ifdef NSIS_SUPPORT_EXECUTE
  definedlist.add("NSIS_SUPPORT_EXECUTE");
#endif
#ifdef NSIS_SUPPORT_SHELLEXECUTE
  definedlist.add("NSIS_SUPPORT_SHELLEXECUTE");
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
#ifdef NSIS_SUPPORT_ENVIRONMENT
  definedlist.add("NSIS_SUPPORT_ENVIRONMENT");
#endif
#ifdef NSIS_SUPPORT_RMDIR
  definedlist.add("NSIS_SUPPORT_RMDIR");
#endif
#ifdef NSIS_SUPPORT_FILE
  definedlist.add("NSIS_SUPPORT_FILE");
#endif
#ifdef NSIS_SUPPORT_DELETE
  definedlist.add("NSIS_SUPPORT_DELETE");
#endif
#ifdef NSIS_SUPPORT_RENAME
  definedlist.add("NSIS_SUPPORT_RENAME");
#endif
#ifdef NSIS_SUPPORT_MESSAGEBOX
  definedlist.add("NSIS_SUPPORT_MESSAGEBOX");
#endif
#ifdef NSIS_SUPPORT_CODECALLBACKS
  definedlist.add("NSIS_SUPPORT_CODECALLBACKS");
#endif
#ifdef NSIS_SUPPORT_MOVEONREBOOT
  definedlist.add("NSIS_SUPPORT_MOVEONREBOOT");
#endif

  {
    char b[123];
    wsprintf(b,"%d",NSIS_MAX_STRLEN);
    definedlist.add("NSIS_MAX_STRLEN",b);
    wsprintf(b,"%d",NSIS_MAX_INST_TYPES);
    definedlist.add("NSIS_MAX_INST_TYPES",b);
  }

  // Added by Amir Szekely 11th July 2002
  // Coded by Robert Rainwater
  {
    char szNSISDir[NSIS_MAX_STRLEN],*fn2;
    GetModuleFileName(NULL,szNSISDir,sizeof(szNSISDir));
    fn2=strrchr(szNSISDir,'\\');
    if(fn2!=NULL) *fn2=0;
    definedlist.add("NSISDIR",(char*)szNSISDir);
  }

  db_opt_save=db_comp_save=db_full_size=db_opt_save_u=db_comp_save_u=db_full_size_u=0;

  // Added by Amir Szekely 31st July 2002
  compressor = &zlib_comressor;
  build_compressor_set = false;
#ifdef NSIS_ZLIB_COMPRESS_WHOLE
  build_compress_whole = true;
#else
  build_compress_whole = false;
#endif

  cur_entries=&build_entries;
  cur_datablock=&build_datablock;
  cur_functions=&build_functions;
  cur_labels=&build_labels;

  subsection_open_cnt=0;
  build_cursection_isfunc=0;
  build_cursection=NULL;
  // init public data.
  build_packname[0]=build_packcmd[0]=build_output_filename[0]=0;

  build_overwrite=0;
  build_compress=1;
  build_crcchk=1; 
  build_datesave=1;
  build_optimize_datablock=1;

  memset(&build_header,-1,sizeof(build_header));

  build_header.install_reg_rootkey=0;
#ifdef NSIS_CONFIG_COMPONENTPAGE
  build_header.no_custom_instmode_flag=0;
#endif
  build_header.num_sections=0;
  build_header.space_avail_id=0;
  build_header.space_req_id=0;
  build_header.dir_subtext_id=0;
  build_header.com_subtext1_id=0;
  build_header.com_subtext2_id=0;
  build_header.common.intro_text_id=0;
  build_header.common.num_entries=0;
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  build_header.common.silent_install=0;
#endif
  build_header.common.misc_flags=8;
  build_header.common.show_details=0;
  build_header.common.lb_bg=RGB(0,0,0);
  build_header.common.lb_fg=RGB(0,255,0);
  build_header.common.progress_flags=0;

  uninstall_mode=0;
  uninstall_size_full=0;
  uninstall_size=-1;

  memset(&build_uninst,-1,sizeof(build_uninst));
  build_uninst.uninst_subtext_id=0;
  build_uninst.common.intro_text_id=0;
  build_uninst.common.lb_bg=RGB(0,0,0);
  build_uninst.common.lb_fg=RGB(0,255,0);
  build_uninst.common.num_entries=0;
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  build_uninst.common.silent_install=0;
#endif
  build_uninst.code=0;
  build_uninst.code_size=-1;
  build_uninst.common.show_details=0;
  build_uninst.common.misc_flags=0;

  uninstaller_writes_used=0;

  // Changed by Amir Szekely 11th July 2002
  // Changed to fit the new format in which uninstaller icons are saved
  m_unicon_data=(unsigned char *)malloc(unicondata_size+3*sizeof(DWORD));
  memcpy(m_unicon_data+3*sizeof(DWORD),unicon_data+22,unicondata_size);
  *(DWORD*)m_unicon_data = 1;
  *(DWORD*)(DWORD(m_unicon_data) + sizeof(DWORD)) = unicondata_size;
  *(DWORD*)(DWORD(m_unicon_data) + 2*sizeof(DWORD)) = 0;
  unicondata_size += 3*sizeof(DWORD);

  m_inst_fileused=0;
  m_uninst_fileused=0;

  branding_image_found=false; // Added by Amir Szekely 22nd July 2002
  
  no_space_texts=false;
}

int CEXEBuild::getcurdbsize() { return cur_datablock->getlen(); }

int CEXEBuild::add_string(const char *string) // returns offset in stringblock
{
  if (uninstall_mode) return add_string_uninst(string,1);
  return add_string_main(string,1);
}

// based on Dave Laundon's code
int CEXEBuild::preprocess_string(char *out, const char *in)
{
  static const char VarNames[] =
  "HWNDPARENT\0"    // 0
  "0\0"             // 1
  "1\0"             // 2
  "2\0"             // 3
  "3\0"             // 4
  "4\0"             // 5
  "5\0"             // 6
  "6\0"             // 7
  "7\0"             // 8
  "8\0"             // 9
  "9\0"             // 10
  "R0\0"            // 11
  "R1\0"            // 12
  "R2\0"            // 13
  "R3\0"            // 14
  "R4\0"            // 15
  "R5\0"            // 16
  "R6\0"            // 17
  "R7\0"            // 18
  "R8\0"            // 19
  "R9\0"            // 20
  "CMDLINE\0"       // 21 everything before here doesn't have trailing slash removal

  "INSTDIR\0"       // 22
  "OUTDIR\0"        // 23
  "EXEDIR\0"        // 24
  "PROGRAMFILES\0"  // 25
  "SMPROGRAMS\0"    // 26
  "SMSTARTUP\0"     // 27
  "DESKTOP\0"       // 28
  "STARTMENU\0"     // 29
  "QUICKLAUNCH\0"   // 30
  "TEMP\0"          // 31
  "WINDIR\0"        // 32
  "SYSDIR\0"        // 33
  "LANGUAGE\0"      // 34
  ;

  const char *p=in;
  while (*p) 
  {
    const char *np=CharNext(p);

    if (np-p > 1) // multibyte char
    {
      int l=np-p;
      while (l--)
      {
        int i = (unsigned char)*p++;
        if (i >= VAR_CODES_START) {
          *out++ = (char)255;
        }
        *out++=i;
      }
      continue;
    }

    int i = (unsigned char)*p;

    p=np;

    // Test for characters extending into the variable codes
    if (i >= VAR_CODES_START) {
      *out++ = (char)255;
    }
    else if (i == '$') 
    {
      if (*p == '$') 
        p++; // Can simply convert $$ to $ now
      else 
      {
        const char *pVarName;
        for (
          pVarName = VarNames, i = VAR_CODES_START;
          strncmp(pVarName, p, strlen(pVarName));
          pVarName += strlen(pVarName) + 1, i++
        );
        // Found?
        if (*pVarName) p += strlen(pVarName);
        else  // warning should go here
        {
          char tbuf[64];
          strncpy(tbuf,p,63);
          tbuf[63]=0;
          if (strstr(tbuf," ")) strstr(tbuf," ")[0]=0;
          warning("unknown variable \"%s\" detected, ignoring\n",tbuf);
          i = '$';
        }
      }
    }
    *out++=i;
  }
  *out=0;
  return 0;
}

int CEXEBuild::add_string_main(const char *string, int process) // returns offset (in string block)
{
  if (!*string) return -1;
  if (!process) return build_strlist.add(string,2);

  char buf[4096];
  preprocess_string(buf,string);
  return build_strlist.add(buf,2);
}

int CEXEBuild::add_string_uninst(const char *string, int process) // returns offset (in string block)
{
  if (!*string) return -1;
  if (!process) return ubuild_strlist.add(string,2);

  char buf[4096];
  preprocess_string(buf,string);
  return ubuild_strlist.add(buf,2);
}

// what it does is, when you pass it the offset of the last item added, it will determine if 
// that data is already present in the datablock, and if so, reference it instead (and shorten
// the datablock as necessary). Reduces overhead if you want to add files to a couple places.
// Woo, an optimizing installer generator, now we're styling.

int CEXEBuild::datablock_optimize(int start_offset)
{
  int this_len = cur_datablock->getlen()-start_offset;
  int pos=0;

  if (!build_optimize_datablock) return start_offset;

  char *db=(char*)cur_datablock->get();
  int first_int=*(int*)(db+start_offset);
  if (this_len >= 4) while (pos < start_offset)
  {
    int this_int = *(int*)(db+pos);
    if (this_int == first_int && !memcmp(db+pos,db+start_offset,this_len))
    {
      db_opt_save+=this_len;
      cur_datablock->resize(max(start_offset,pos+this_len));
      return pos;
    }
    pos += 4 + (this_int&0x7fffffff);
  }

  return start_offset;
}

int CEXEBuild::add_data(const char *data, int length, IGrowBuf *dblock) // returns offset
{
  // Changed by Amir Szekely 31st July 2002
  // Ability to change compression methods from within the script
  build_compressor_set=true;

  int done=0;

  if (length < 0)
  {
    ERROR_MSG("Error: add_data() called with length=%d\n",length);
    return -1;
  }

  if (!dblock) dblock=cur_datablock;

  int st=dblock->getlen();

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  if (!build_compress_whole && build_compress)
  {
    // grow datablock so that there is room to compress into
    int bufferlen=length+1024+length/4; // give a nice 25% extra space
    dblock->resize(st+bufferlen+sizeof(int));

    int n;
    if ((n=compressor->Init(9)) != C_OK) 
    {
      ERROR_MSG("Internal compiler error #12345: deflateInit() failed(%d).\n",n);
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
      if (dblock == cur_datablock)
      {
        int nst=datablock_optimize(st);
        if (nst == st) db_comp_save+=length-used;
        else st=nst;
      }
    }
    compressor->End();
  }
#endif // NSIS_CONFIG_COMPRESSION_SUPPORT

  if (!done)
  {
    dblock->resize(st);
    dblock->add(&length,sizeof(int));
    dblock->add(data,length);
    if (dblock == cur_datablock)
    {
      st=datablock_optimize(st);
    }
  }

  if (dblock == cur_datablock)
  {
    db_full_size += length + sizeof(int);
  }

  return st;
}

int CEXEBuild::add_label(const char *name)
{
  if (!build_cursection && !uninstall_mode)
  {
    ERROR_MSG("Error: Label declaration not valid outside of function/section\n"); 
    return PS_ERROR;
  }
  if ((name[0] >= '0' && name[0] <= '9') || name[0] == '-' || name[0] == ' ' || name[0] == ':')
  {
    ERROR_MSG("Error: labels must not begin with 0-9, -, :, or a space.\n"); 
    return PS_ERROR;
  }
  int cs;
  int ce;
  if (build_cursection)
  {
    cs=build_cursection->code;
    ce=cs+build_cursection->code_size;
  }
  else
  {
    cs=build_uninst.code;
    ce=cs+build_uninst.code_size;
  }

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
  
  section s={0,};
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
  if (!funname[0])
  {
    ERROR_MSG("Error: Function must have a name\n");
    return PS_ERROR;
  }

  if (!strnicmp(funname,"un.",3))
  {
    set_uninstall_mode(1);
  }

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
  build_cursection->default_state=0;
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
  build_cursection_isfunc=0;
  build_cursection=NULL;

  // add ret.
  entry ent={EW_RET,};
  cur_entries->add(&ent,sizeof(entry));
  if (!uninstall_mode) build_header.common.num_entries++;
  else build_uninst.common.num_entries++;

  set_uninstall_mode(0);
  return PS_OK;
}


int CEXEBuild::section_add_flags(int flags)
{
  if (uninstall_mode)
  {
    ERROR_MSG("Error: can't modify flags of uninstall section\n");
    return PS_ERROR;
  }
  if (!build_cursection || build_cursection_isfunc)
  {
    ERROR_MSG("Error: can't modify flags when no section is open\n");
    return PS_ERROR;
  }
  build_cursection->default_state|=flags;
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
  else if (uninstall_mode)
  {
    entry ent={EW_RET,};
    cur_entries->add(&ent,sizeof(entry));
    build_uninst.common.num_entries++;
    set_uninstall_mode(0);
  }
  else if (!build_cursection)
  {
    ERROR_MSG("Error: SectionEnd specified and no sections open\n");
    return PS_ERROR;
  }
  else 
  {
    entry ent={EW_RET,};
    cur_entries->add(&ent,sizeof(entry));
    build_header.common.num_entries++;
  }
  build_cursection=NULL;
  return PS_OK;
}

int CEXEBuild::add_section(const char *secname, const char *file, int line, const char *defname, int expand=0)
{
  if (build_cursection_isfunc)
  {
    ERROR_MSG("Error: Section can't create section (already in function, use FunctionEnd first)\n");
    return PS_ERROR;
  }
  if (build_cursection || uninstall_mode)
  {
    ERROR_MSG("Error: Section already open, call SectionEnd first\n");
    return PS_ERROR;
  }
  if (!stricmp(secname,"uninstall"))
  {
    if (defname[0])
    {
      ERROR_MSG("Error: Uninstall section cannot have index output define\n");
      return PS_ERROR;
    }
    if (build_uninst.code_size >= 0)
    {
      ERROR_MSG("Error: Uninstall section already specified\n");
      return PS_ERROR;
    }
    if (subsection_open_cnt)
    {
      ERROR_MSG("Error: Uninstall section specified inside SubSection\n");
      return PS_ERROR;
    }
    build_uninst.code=ubuild_entries.getlen()/sizeof(entry);
    build_uninst.code_size=0;
    set_uninstall_mode(1);
    build_cursection=NULL;
    return PS_OK;
  }

  int n=(build_sections.getlen()/sizeof(section));
  build_sections.resize(build_sections.getlen()+sizeof(section));
  build_cursection=((section*)build_sections.get()) + n;
  build_cursection->default_state=DFS_SET;
  build_cursection->name_ptr=add_string(secname);
  build_cursection->code=cur_entries->getlen()/sizeof(entry);
  build_cursection->code_size=0;
  build_cursection->size_kb=0;
  build_cursection->expand=expand;

  if (secname[0]=='-')
  {
    build_cursection=NULL;
    entry ent={EW_RET,}; 
    cur_entries->add(&ent,sizeof(entry)); 
    build_header.common.num_entries++; 
  }

  if (defname[0])
  {
    char buf[32];
    wsprintf(buf,"%d",build_header.num_sections);
    if (definedlist.add(defname,buf))
    {
      ERROR_MSG("Error: \"%s\" already defined, can't assign section index!\n",defname);
      return PS_ERROR;
    }
  }
  
  build_header.num_sections++;

  return PS_OK;
}

int CEXEBuild::make_sure_not_in_secorfunc(const char *str)
{
  if (build_cursection || uninstall_mode)
  {
    ERROR_MSG("Error: command %s not valid in %s\n",str,build_cursection_isfunc?"function":"section");
    return PS_ERROR;
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

  if (!uninstall_mode)
  {
    if (!build_cursection_isfunc && build_cursection->name_ptr >=0 && build_strlist.get()[build_cursection->name_ptr] == '-')
    {
      ERROR_MSG("Error: cannot add entry to divider section\n");
      return PS_ERROR;
    }
    build_cursection->code_size++;
    build_header.common.num_entries++;
  }
  else
  {
    build_uninst.common.num_entries++;
    if (build_cursection) build_cursection->code_size++;
    else build_uninst.code_size++;
  }

  return PS_OK;
}

int CEXEBuild::resolve_jump_int(const char *fn, int *a, int offs, int start, int end)
{
  if (*a > 0) 
  {
    char *lname=(char*)ns_label.get()+*a;
    if (lname[0] == '-' || lname[0]=='+')
    {
      *a=offs+atoi(lname)+1;
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
          s->default_state++;
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
      sec->default_state++;
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
  else if (w->which == EW_MESSAGEBOX)
  {
    if (resolve_jump_int(fn,&w->offsets[3],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[4],offs,start,end)) return 1;
  }
  else if (w->which == EW_IFFILEEXISTS)
  {
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[2],offs,start,end)) return 1;
  }
  else if (w->which == EW_IFERRORS)
  {
    if (resolve_jump_int(fn,&w->offsets[0],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
  }
  else if (w->which == EW_IFREBOOTFLAG)
  {
    if (resolve_jump_int(fn,&w->offsets[0],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
  }
  else if (w->which == EW_STRCMP)
  {
    if (resolve_jump_int(fn,&w->offsets[2],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[3],offs,start,end)) return 1;
  }
  else if (w->which == EW_INTCMP || w->which == EW_INTCMPU)
  {
    if (resolve_jump_int(fn,&w->offsets[2],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[3],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[4],offs,start,end)) return 1;
  }
  else if (w->which == EW_ISWINDOW)
  {
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
    if (resolve_jump_int(fn,&w->offsets[2],offs,start,end)) return 1;
  }
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
  else if (w->which == EW_GETFUNCTIONADDR)
  {
    char buf[32];
    if (w->offsets[1] < 0)
    {
      ERROR_MSG("Error: GetFunctionAddress requires a real function to get address of.\n");
      return 1;
    }
    
    if (resolve_call_int(fn,str,w->offsets[1],&w->offsets[1])) return 1;

    w->which=EW_ASSIGNVAR;
    wsprintf(buf,"%d",w->offsets[1]+1); // +1 here to make 1-based.
    w->offsets[1]=add_string(buf);
  }
  else if (w->which == EW_GETLABELADDR)
  {
    char buf[32];
    if (resolve_jump_int(fn,&w->offsets[1],offs,start,end)) return 1;
    w->which=EW_ASSIGNVAR;
    wsprintf(buf,"%d",w->offsets[1]);
    w->offsets[1]=add_string(buf);
  }
  return 0;
}

int CEXEBuild::resolve_coderefs(const char *str)
{
  // resolve jumps&calls
  {
    section *sec=(section *)cur_functions->get();    
    int l=cur_functions->getlen()/sizeof(section);
    entry *w=(entry*)cur_entries->get();
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
    if (uninstall_mode)
    {
      int x;
      for (x = build_uninst.code; x < build_uninst.code+build_uninst.code_size; x ++)
        if (resolve_instruction("\"uninstall section\"",str,w+x,x,build_uninst.code,build_uninst.code+build_uninst.code_size)) return 1;
    }
    else
    {
      int cnt=0;
      sec=(section *)build_sections.get();
      l=build_sections.getlen()/sizeof(section);
      while (l-- > 0)
      {
        int x;
        for (x = sec->code; x < sec->code+sec->code_size; x ++)
        {
          char fname[1024];
          if (sec->name_ptr>=0) wsprintf(fname,"section \"%s\" (%d)",build_strlist.get()+sec->name_ptr,cnt);
          else wsprintf(fname,"unnamed section (%d)",cnt);
          if (resolve_instruction(fname,str,w+x,x,sec->code,sec->code+sec->code_size)) return 1;
        }
        sec++;
        cnt++;
      }
    }
  }

  // optimize unused functions
  {
    section *sec=(section *)cur_functions->get();    
    int l=cur_functions->getlen()/sizeof(section);
    entry *w=(entry*)cur_entries->get();
    while (l-- > 0)
    {
      if (sec->name_ptr)
      {
        if (!sec->default_state)
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
      if (!t->default_state)
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

int CEXEBuild::write_output(void)
{
#ifndef NSIS_CONFIG_CRC_SUPPORT
  build_crcchk=0;
#endif
  if (has_called_write_output)
  {
    ERROR_MSG("Error (write_output): write_output already called, can't continue\n");
    return PS_ERROR;
  }
  has_called_write_output++;
  if (!build_output_filename[0])
  {
    ERROR_MSG("Error: invalid script: never had OutFile command\n");
    return PS_ERROR;
  }

  {
    int ns=build_sections.getlen()/sizeof(section);
    if (!ns)
    {
      ERROR_MSG("Error: invalid script: no sections specified\n");
      return PS_ERROR;
    }

  // Added by Amir Szekely 3rd August 2002
  if (WriteStringTables() == PS_ERROR) return PS_ERROR;

/*#ifdef NSIS_CONFIG_COMPONENTPAGE
    if (build_header.componenttext_ptr < 0 &&
        ns > 1
#ifdef NSIS_CONFIG_SILENT_SUPPORT
        && !build_header.common.silent_install
#endif
        )
    {
      build_header.componenttext_ptr=-1;
    }
#endif*/
  }

  if (!build_entries.getlen())
  {
    ERROR_MSG("Error: invalid script: no entries specified\n");
    return PS_ERROR;
  }

  if (build_cursection || uninstall_mode)
  {
    ERROR_MSG("Error: Section left open at EOF\n");
    return PS_ERROR;
  } 

  if (subsection_open_cnt)
  {
    ERROR_MSG("Error: SubSection left open at EOF\n");
    return PS_ERROR;
  }

  // deal with functions, for both install and uninstall modes.
  if (build_cursection_isfunc)
  {
    ERROR_MSG("Error: Function still open at EOF, cannot proceed\n");
    return 1;
  }

#ifdef NSIS_CONFIG_LICENSEPAGE
  if (build_header.license_bg<0)
    build_header.license_bg=GetSysColor(COLOR_BTNFACE);
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (ubuild_entries.getlen())
  {
    if (!uninstaller_writes_used)
    {
      warning("Uninstall turned on but WriteUninstaller never used. No uninstaller will be created.");
    }
    else
    {
      build_uninst.common.progress_flags=build_header.common.progress_flags;
      build_uninst.common.misc_flags|=build_header.common.misc_flags&(4|8);
      
      set_uninstall_mode(1);
  #ifdef NSIS_SUPPORT_CODECALLBACKS
      if (resolve_call_int("uninstall callback","un.callbacks",ns_func.find("un.onInit",0),&build_uninst.common.code_onInit)) return PS_ERROR;
      if (resolve_call_int("uninstall callback","un.callbacks",ns_func.find("un.onUninstSuccess",0),&build_uninst.common.code_onInstSuccess)) return PS_ERROR;
      if (resolve_call_int("uninstall callback","un.callbacks",ns_func.find("un.onUninstFailed",0),&build_uninst.common.code_onInstFailed)) return PS_ERROR;
      if (resolve_call_int("uninstall callback","un.callbacks",ns_func.find("un.onUserAbort",0),&build_uninst.common.code_onUserAbort)) return PS_ERROR;
      if (resolve_call_int("uninstall callback","un.callbacks",ns_func.find("un.onNextPage",0),&build_uninst.common.code_onNextPage)) return PS_ERROR;
  #endif//NSIS_SUPPORT_CODECALLBACKS
      if (resolve_coderefs("uninstall")) return PS_ERROR;
      set_uninstall_mode(0);
    }
  }
  else if (uninstaller_writes_used)
  {
    ERROR_MSG("Error: no Uninstall section specified, but WriteUninstaller used %d time(s)\n",uninstaller_writes_used);
    return PS_ERROR;
  }
#endif


#ifdef NSIS_SUPPORT_CODECALLBACKS
  if (resolve_call_int("install callback",".callbacks",ns_func.find(".onInit",0),&build_header.common.code_onInit)) return PS_ERROR;
  if (resolve_call_int("install callback",".callbacks",ns_func.find(".onInstSuccess",0),&build_header.common.code_onInstSuccess)) return PS_ERROR;
  if (resolve_call_int("install callback",".callbacks",ns_func.find(".onInstFailed",0),&build_header.common.code_onInstFailed)) return PS_ERROR;
  if (resolve_call_int("install callback",".callbacks",ns_func.find(".onUserAbort",0),&build_header.common.code_onUserAbort)) return PS_ERROR;
  if (resolve_call_int("install callback",".callbacks",ns_func.find(".onVerifyInstDir",0),&build_header.code_onVerifyInstDir)) return PS_ERROR;
  if (resolve_call_int("install callback",".callbacks",ns_func.find(".onNextPage",0),&build_header.common.code_onNextPage)) return PS_ERROR;
  if (resolve_call_int("install callback",".callbacks",ns_func.find(".onPrevPage",0),&build_header.code_onPrevPage)) return PS_ERROR;
#ifdef NSIS_CONFIG_COMPONENTPAGE
  if (resolve_call_int("install callback",".callbacks",ns_func.find(".onSelChange",0),&build_header.code_onSelChange)) return PS_ERROR;
#endif//NSIS_CONFIG_COMPONENTPAGE
#endif//NSIS_SUPPORT_CODECALLBACKS
  
  if (resolve_coderefs("install")) return PS_ERROR;

  // Added by Amir Szekely 8th July 2002
  // Removes any unused resources
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  try {
  	SCRIPT_MSG("Removing unused resources... ");
    CResourceEditor re(header_data_new, exeheader_size_new);
#ifdef NSIS_CONFIG_LICENSEPAGE
    if (IsNotSet(installer.licensedata)
#ifdef NSIS_CONFIG_SILENT_SUPPORT
      || build_header.common.silent_install
#endif // NSIS_CONFIG_SILENT_SUPPORT
     )
    {
      re.UpdateResource(RT_DIALOG, IDD_LICENSE, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), 0, 0);
    }
#endif // NSIS_CONFIG_LICENSEPAGE
#ifdef NSIS_CONFIG_COMPONENTPAGE
    if (IsNotSet(installer.componenttext)
#ifdef NSIS_CONFIG_SILENT_SUPPORT
      || build_header.common.silent_install
#endif // NSIS_CONFIG_SILENT_SUPPORT
     )
    {
      re.UpdateResource(RT_DIALOG, IDD_SELCOM, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), 0, 0);
      re.UpdateResource(RT_BITMAP, IDB_BITMAP1, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), 0, 0);
    }
#endif // NSIS_CONFIG_COMPONENTPAGE
    if (IsNotSet(installer.text)
#ifdef NSIS_CONFIG_SILENT_SUPPORT
      || build_header.common.silent_install
#endif // NSIS_CONFIG_SILENT_SUPPORT
     )
    {
      re.UpdateResource(RT_DIALOG, IDD_DIR, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), 0, 0);
    }
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (!uninstaller_writes_used
#ifdef NSIS_CONFIG_SILENT_SUPPORT
      || build_uninst.common.silent_install
#endif // NSIS_CONFIG_SILENT_SUPPORT
     )
    {
      re.UpdateResource(RT_DIALOG, IDD_UNINST, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), 0, 0);
    }
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT
#ifdef NSIS_CONFIG_SILENT_SUPPORT
    if (build_header.common.silent_install
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
        && (build_uninst.common.silent_install || !uninstaller_writes_used)
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT
     )
    {
      re.UpdateResource(RT_DIALOG, IDD_INST, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), 0, 0);
      re.UpdateResource(RT_DIALOG, IDD_INSTFILES, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), 0, 0);
#ifdef NSIS_CONFIG_CRC_SUPPORT
      re.UpdateResource(RT_DIALOG, IDD_VERIFY, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), 0, 0);
#endif // NSIS_CONFIG_CRC_SUPPORT
    }
#endif // NSIS_CONFIG_SILENT_SUPPORT

    free(header_data_new);
    header_data_new = re.Save((DWORD&)exeheader_size_new);

    SCRIPT_MSG("Done!\n");
  }
  catch (exception& err) {
    ERROR_MSG("\nError: %s\n", err.what());
    return PS_ERROR;
  }
#endif // NSIS_CONFIG_VISIBLE_SUPPORT

  // Pack exe header if asked for
  if (build_packname[0] && build_packcmd[0])
  {
    FILE *tmpfile=fopen(build_packname,"wb");
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
    tmpfile=fopen(build_packname,"rb");
    if (!tmpfile)
    {
      remove(build_packname);
      ERROR_MSG("Error: reading temporary file \"%s\" after pack\n",build_packname);
      return PS_ERROR;
    }
    fseek(tmpfile,0,SEEK_END);
    exeheader_size_new=ftell(tmpfile);
    exeheader_size_new+=511;
    exeheader_size_new&=~511; // align to 512.
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

    // write out exe header, pack, read back in, align to 512, and
    // update the header info
  }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  // Get offsets of icons to replace for uninstall
  // Also makes sure that the icons are there and in the right size.
  if (uninstaller_writes_used) {
    SCRIPT_MSG("Finding icons offsets for uninstaller... ");
    icon_offset = generate_unicons_offsets(header_data_new, m_unicon_data);
    if (icon_offset == 0)
      return PS_ERROR;
	  SCRIPT_MSG("Done!\n");
  }
#endif //NSIS_CONFIG_UNINSTALL_SUPPORT

  build_optimize_datablock=0;

  if (uninstall_generate() != PS_OK) 
  {
    return PS_ERROR;
  }

  int crc=0;

  {
    char buffer[1024],*p;
    GetFullPathName(build_output_filename,1024,buffer,&p);
    INFO_MSG("\nOutput: \"%s\"\n",buffer);
  }
  FILE *fp = fopen(build_output_filename,"w+b");
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

  firstheader fh={0,};
  fh.nsinst[0]=FH_INT1;
  fh.nsinst[1]=FH_INT2;
  fh.nsinst[2]=FH_INT3;

  fh.flags=(build_crcchk?FH_FLAGS_CRC:0);
  fh.flags|=(build_crcchk==2?FH_FLAGS_FORCE_CRC:0);
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  if (build_header.common.silent_install) fh.flags |= FH_FLAGS_SILENT;
#endif
  fh.siginfo=FH_SIG;

  int installinfo_compressed;
  int fd_start;
  if (build_compress_whole)
  {
    if ((compressor->Init(9)) != C_OK)
    {
      ERROR_MSG("Error: deflateInit() returned < 0\n");
      return PS_ERROR;
    }
  }
  {
    GrowBuf ihd;
    {
      GrowBuf hdrcomp;
      hdrcomp.add(&build_header,sizeof(build_header)); 
      hdrcomp.add(build_sections.get(),build_sections.getlen()); 
      hdrcomp.add(build_entries.get(),build_entries.getlen());
      hdrcomp.add(build_strlist.get(),build_strlist.getlen());

      if (add_data((char*)hdrcomp.get(),hdrcomp.getlen(),&ihd) < 0) return PS_ERROR;
 
      installinfo_compressed=ihd.getlen();
      fh.length_of_header=hdrcomp.getlen();
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

    if (build_compress_whole) {
      if (deflateToFile(fp,(char*)ihd.get(),ihd.getlen()))
      {
        fclose(fp);
        return PS_ERROR;
      }
    }
    else {
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

  {
    int ns=build_sections.getlen()/sizeof(section);
    section *s=(section*)build_sections.get();
    int x;
    int req=1;
    int div=0;
    int divptr=build_strlist.find("-",2);
    for (x = 1; x < ns; x ++)
    {
      if (s[x].name_ptr == divptr) div++;
      if (s[x].name_ptr == -1)  req++;
    }
    INFO_MSG("Install: %d section%s",ns,ns==1?"":"s");
    if (req||div) 
    {
      INFO_MSG(" (");
      if (req) 
      {
        INFO_MSG("%d required",req);
        if (div) INFO_MSG(", ");
      }
      if (div) INFO_MSG("%d divider%s",div,div==1?"":"s");
      INFO_MSG(")");
    }
    INFO_MSG(".\n");
  }
  int ne=build_entries.getlen()/sizeof(entry);
  INFO_MSG("Install: %d instruction%s (%d bytes), ",ne,ne==1?"":"s",ne*sizeof(entry));
  INFO_MSG("%d byte string table.\n",build_strlist.getlen());
  if (ubuild_entries.getlen()) 
  {
    int tmp=ubuild_entries.getlen()/sizeof(entry);
    INFO_MSG("Uninstall: ");
    INFO_MSG("%d instruction%s (%d bytes), ",tmp,tmp==1?"":"s",tmp*sizeof(entry));
    INFO_MSG("%d byte string table.\n",ubuild_strlist.getlen());
  }


  if (db_opt_save) 
  {
    int total_out_size_estimate=
      exeheader_size_new+sizeof(fh)+build_datablock.getlen()+(build_crcchk?4:0);
    int pc=MulDiv(db_opt_save,1000,db_opt_save+total_out_size_estimate);
    INFO_MSG("Datablock optimizer saved %d bytes (~%d.%d%%).\n",db_opt_save,
      pc/10,pc%10);
  }

  INFO_MSG("\n");
  int total_usize=exeheader_size;

  INFO_MSG("EXE header size:          %10d / %d bytes\n",exeheader_size_new,exeheader_size);

  if (build_compress_whole) {
    INFO_MSG("Install code+strings:                  (%d bytes)\n",
      sizeof(fh)+fh.length_of_header+sizeof(int)); 
  }
  else {
    INFO_MSG("Install code+strings:     %10d / %d bytes\n",
      sizeof(fh)+installinfo_compressed,
      sizeof(fh)+fh.length_of_header+sizeof(int)); 
  }

  total_usize+=sizeof(fh)+fh.length_of_header+sizeof(int);

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
      INFO_MSG("Uninstall code+data+strings:           (%d bytes)\n",uninstall_size_full);
    else
      INFO_MSG("Uninstall code+data+strings:  %6d / %d bytes\n",uninstall_size,uninstall_size_full);
    total_usize+=uninstall_size_full;
  }

  if (build_datablock.getlen())
  {
    char *dbptr=(char *)build_datablock.get();
    int dbl=build_datablock.getlen();
    if (build_compress_whole) {
      INFO_MSG("Compressed data:          ");
//    INFO_MSG("[compress]");
    }
    while (dbl > 0)
    {
      int l=dbl;
      if (l > 32768) l=32768;
      if (build_compress_whole) {
        if (deflateToFile(fp,dbptr,l))
        {
          fclose(fp);
          return PS_ERROR;
        }
      }
      else {
#ifdef NSIS_CONFIG_CRC_SUPPORT
        crc=CRC32(crc,(unsigned char *)dbptr,l);
#endif
        if ((int)fwrite(dbptr,1,l,fp) != l)
        {
          ERROR_MSG("Error: can't write %d bytes to output\n",l);
          fclose(fp);
          return PS_ERROR;
        }
      }
      dbptr+=l;
      dbl-=l;
    }
  }
  if (build_compress_whole) {
    if (deflateToFile(fp,NULL,0))
    {
      fclose(fp);
      return PS_ERROR;
    }
    compressor->End();

    fh.length_of_all_following_data=(ftell(fp)-fd_start)+(build_crcchk?sizeof(int):0);
    INFO_MSG("%10d / %d bytes\n",(ftell(fp)-fd_start),build_datablock.getlen());

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
    fseek(fp,0,SEEK_END); // reset eof flag
  }

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
    int pc=MulDiv(ftell(fp),1000,total_usize);
    INFO_MSG("Total size:               %10d / %d bytes (%d.%d%%)\n",
      ftell(fp),total_usize,pc/10,pc%10);
  }
  fclose(fp);
  print_warnings();
  return PS_OK;
}

int CEXEBuild::deflateToFile(FILE *fp, char *buf, int len) // len==0 to flush
{
  // Changed by Amir Szekely 31st July 2002
  // Ability to change compression methods from within the script
  build_compressor_set=true;

  char obuf[32768];
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
      ERROR_MSG("Error: deflateToFile: deflate()=%d\n",ret);
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
    }
    if (!compressor->GetAvailIn() && (!flush || !l)) break;
  }
  return 0;
}

int CEXEBuild::uninstall_generate()
{
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (ubuild_entries.getlen() && uninstaller_writes_used) 
  {
    firstheader fh={0,};

    GrowBuf uhd;
    // add one more bit (the code+strtabs) to the uninstall datablock
    {
      GrowBuf udata;

      udata.add(&build_uninst,sizeof(build_uninst));
      udata.add(ubuild_entries.get(),ubuild_entries.getlen());
      udata.add(ubuild_strlist.get(),ubuild_strlist.getlen()); 
    
      set_uninstall_mode(1);
      fh.length_of_header=udata.getlen();
      int err=add_data((char*)udata.get(),udata.getlen(),&uhd);      
      set_uninstall_mode(0);
      if (err < 0) return PS_ERROR;
    }
        
    int crc=0;

    build_header.uninstdata_offset=build_datablock.getlen();
    build_header.uninsticon_size=unicondata_size;

    if (add_data((char *)m_unicon_data,unicondata_size) < 0)
      return PS_ERROR;
#ifdef NSIS_CONFIG_CRC_SUPPORT
    #ifdef NSIS_CONFIG_CRC_ANAL
      crc=CRC32(crc,header_data_new,icon_offset);
    #else
      crc=CRC32(crc,header_data_new+512,icon_offset-512);
    #endif
    // Changed by Amir Szekely 11th July 2002
    // This bunch of lines do CRC for the uninstaller icon data
    unsigned char* seeker = m_unicon_data + sizeof(DWORD);
    DWORD dwEndOfIcons = 0;
    for (DWORD i = 0; i < *(DWORD*)m_unicon_data; i++) {
      DWORD dwSize = *(DWORD*)seeker;
      seeker += sizeof(DWORD);
      DWORD dwOffset = *(DWORD*)seeker;
      seeker += sizeof(DWORD);
      // Do CRC for icon data
      crc=CRC32(crc,seeker,dwSize);
      seeker += dwSize;
      if (i < (*(DWORD*)m_unicon_data) - 1) {
        // Do CRC for data between icons
        crc=CRC32(crc,header_data_new+dwOffset+dwSize,(*(DWORD*)(seeker+sizeof(DWORD)))-dwOffset-dwSize);
      }
      else {
        dwEndOfIcons = dwOffset+dwSize;
      }
    }
    // Done with icons, carry on
    crc=CRC32(crc,header_data_new+dwEndOfIcons,exeheader_size_new-dwEndOfIcons);
#endif
    fh.nsinst[0]=FH_INT1;
    fh.nsinst[1]=FH_INT2;
    fh.nsinst[2]=FH_INT3;
    fh.flags = FH_FLAGS_UNINSTALL | (build_crcchk?FH_FLAGS_CRC:0);
    fh.flags |= (build_crcchk==2?FH_FLAGS_FORCE_CRC:0);
#ifdef NSIS_CONFIG_SILENT_SUPPORT
    if (build_uninst.common.silent_install) fh.flags |= FH_FLAGS_SILENT;
#endif
    fh.siginfo=FH_SIG;
    fh.length_of_all_following_data=
      uhd.getlen()+ubuild_datablock.getlen()+(int)sizeof(firstheader)+(build_crcchk?sizeof(int):0);

    GrowBuf udata;

    if (build_compress_whole) {
      // compress uninstaller too
      udata.add(&fh,sizeof(fh));
      {
        char obuf[32768];
        if ((compressor->Init(9)) != C_OK) 
        {
          ERROR_MSG("Error: deflateInit() returned < 0\n");
          return PS_ERROR;
        }
        int x;
        for (x = 0; x < 2; x ++)
        {
          if (!x)
            compressor->SetNextIn((char*)uhd.get(),uhd.getlen());
          else
            compressor->SetNextIn((char*)ubuild_datablock.get(),ubuild_datablock.getlen());
          while (compressor->GetAvailIn())
          {
            compressor->SetNextOut(obuf,sizeof(obuf));
            compressor->Compress(0);
            if (compressor->GetNextOut()-obuf > 0)
              udata.add(obuf,compressor->GetNextOut()-obuf);
          }
        }
        for (;;)
        {
          compressor->SetNextOut(obuf,sizeof(obuf));
          compressor->Compress(C_FINISH);
          if (compressor->GetNextOut()-obuf > 0)
            udata.add(obuf,compressor->GetNextOut()-obuf);
          else break;
        }     
        compressor->End();
      }

      firstheader *_fh=(firstheader *)udata.get();
      _fh->length_of_all_following_data=udata.getlen()+(build_crcchk?sizeof(int):0);
    }
    else {
      udata.add(&fh,sizeof(fh));
      udata.add(uhd.get(),uhd.getlen());
      udata.add(ubuild_datablock.get(),ubuild_datablock.getlen());
    }
#ifdef NSIS_CONFIG_CRC_SUPPORT
    if (build_crcchk)
    {
      int s=CRC32(crc,(unsigned char *)udata.get(),udata.getlen());
      udata.add(&s,sizeof(int));
    }
#endif

    if (add_data((char*)udata.get(),udata.getlen()) < 0) 
      return PS_ERROR;

    uninstall_size_full=fh.length_of_all_following_data + sizeof(int) + unicondata_size - 32 + sizeof(int);

    // compressed size
    uninstall_size=build_datablock.getlen()-build_header.uninstdata_offset;
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
    if (un) cur_datablock=&ubuild_datablock;
    else cur_datablock=&build_datablock;
    if (un) cur_entries=&ubuild_entries;
    else cur_entries=&build_entries;
    if (un) cur_functions=&ubuild_functions;
    else cur_functions=&build_functions;
    if (un) cur_labels=&ubuild_labels;
    else cur_labels=&build_labels;

    SWAP(db_opt_save_u,db_opt_save,int);
    SWAP(db_comp_save_u,db_comp_save,int);
    SWAP(db_full_size_u,db_full_size,int);
  }
}

extern FILE *g_output;

void CEXEBuild::warning(const char *s, ...)
{
  char buf[4096];
  va_list val;
  va_start(val,s);
  vsprintf(buf,s,val);
  va_end(val);
  m_warnings.add(buf,-1);
  if (display_warnings) 
  {
    fprintf(g_output,"warning: %s\n",buf);
    fflush(g_output);
  }
}

void CEXEBuild::ERROR_MSG(const char *s, ...)
{
  if (display_errors)
  {
    char buf[4096];
    va_list val;
    va_start(val,s);
    vsprintf(buf,s,val);
    va_end(val);
    fprintf(g_output,"%s",buf);
    fflush(g_output);
  }
}

void CEXEBuild::SCRIPT_MSG(const char *s, ...)
{
  if (display_script)
  {
    char buf[4096];
    va_list val;
    va_start(val,s);
    vsprintf(buf,s,val);
    va_end(val);
    fprintf(g_output,"%s",buf);
    fflush(g_output);
  }
}

void CEXEBuild::INFO_MSG(const char *s, ...)
{
  if (display_info)
  {
    char buf[4096];
    va_list val;
    va_start(val,s);
    vsprintf(buf,s,val);
    va_end(val);
    fprintf(g_output,"%s",buf);
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

// Added by Ximon Eighteen 5th August 2002
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
void CEXEBuild::build_plugin_table(void)
{
  char* nsisdir = definedlist.find("NSISDIR");
  if (nsisdir)
  {
    char* searchPath = new char [strlen(nsisdir)+6];
    if (searchPath)
    {
      strcpy(searchPath,nsisdir);
      strcat(searchPath,"\\bin");
      INFO_MSG("\nProcessing plugin dlls: \"%s\\*.dll\"\n",searchPath);
      m_plugins.FindCommands(searchPath,display_info?true:false);
      delete[] searchPath;
    }
  }
}
#endif // NSIS_CONFIG_PLUGIN_SUPPORT