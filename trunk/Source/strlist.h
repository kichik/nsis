#ifndef _STRLIST_H_
#define _STRLIST_H_

#include <stdlib.h> // for gcc

class IGrowBuf 
{
  public:
    virtual int add(const void *data, int len)=0;
    virtual void resize(int newlen)=0;
    virtual int getlen()=0;
    virtual void *get()=0;
};

class GrowBuf : public IGrowBuf
{
  public:
    GrowBuf() { m_alloc=m_used=m_zero=0; m_s=NULL; m_bs=32768; }
    ~GrowBuf() { free(m_s); }

    void set_zeroing(int zero) { m_zero=zero; }

    int add(const void *data, int len) 
    { 
      if (len<=0) return 0;
      resize(m_used+len); 
      memcpy((char*)m_s+m_used-len,data,len);
      return m_used-len;
    }

    void resize(int newlen)
    {
      int os=m_alloc;
      int ou=m_used;
      m_used=newlen;
      if (newlen > m_alloc)
      {
        void *n;
        m_alloc = newlen*2 + m_bs;
        n = realloc(m_s, m_alloc);
        if (!n)
        {
          extern FILE *g_output;
          extern int g_display_errors;
          if (g_display_errors) 
          {
            fprintf(g_output,"\nack! realloc(%d) failed, trying malloc(%d)!\n",m_alloc,newlen);
            fflush(g_output);
          }
          m_alloc=newlen; // try to malloc the minimum needed
          n=malloc(m_alloc);
          if (!n)
          {
            extern void quit();
            if (g_display_errors) 
            {
              fprintf(g_output,"\nInternal compiler error #12345: GrowBuf realloc/malloc(%d) failed.\n",m_alloc);
              fflush(g_output);
            }
            quit();
          }
          memcpy(n,m_s,min(newlen,os));
          free(m_s);
        }
        m_s=n;
        if (m_zero) memset((char*)m_s+ou,0,m_alloc-ou);
      }
      if (!m_used && m_alloc > 2*m_bs) // only free if you resize to 0 and we're > 64k
      {
        m_alloc=0;
        free(m_s);
        m_s=NULL;
      }
    }

    int getlen() { return m_used; }
    void *get() { return m_s; }

  private:
    void *m_s;
    int m_alloc;
    int m_used;
    int m_zero;

  protected:
    int m_bs;
};

class TinyGrowBuf : public GrowBuf {
  public:
    TinyGrowBuf() : GrowBuf() { m_bs=1024; }
};

class StringList
{
public:
  StringList() { }
  ~StringList() { }

  int add(const char *str, int case_sensitive)
  {
    int a=find(str,case_sensitive);
    if (a >= 0 && case_sensitive!=-1) return a;
    return gr.add(str,strlen(str)+1);
  }

  // use 2 for case sensitive end-of-string matches too
  int find(const char *str, int case_sensitive, int *idx=NULL) // returns -1 if not found
  {
    char *s=(char*)gr.get();
    int ml=gr.getlen();
    int offs=0;
    if (idx) *idx=0;
    while (offs < ml)
    {
      if ((case_sensitive && !strcmp(s+offs,str)) ||
          (!case_sensitive && !stricmp(s+offs,str)))
      {
        return offs;
      }
      if (case_sensitive==2 && 
          strlen(str) < strlen(s+offs) &&  // check for end of string
          !strcmp(s+offs+strlen(s+offs)-strlen(str),str))
      {
        return offs+strlen(s+offs)-strlen(str);
      }
      offs+=strlen(s+offs)+1;
      if (idx) (*idx)++;
    }
    return -1;
  }

  void delbypos(int pos)
  {
    char *s=(char*)gr.get();
    int len=strlen(s+pos)+1;
    if (pos+len < gr.getlen()) memcpy(s+pos,s+pos+len,gr.getlen()-(pos+len));
    gr.resize(gr.getlen()-len);
  }

  int idx2pos(int idx)
  {
    char *s=(char*)gr.get();
    int offs=0;
    int cnt=0;
    if (idx>=0) while (offs < gr.getlen())
    {
      if (cnt++ == idx) return offs;
      offs+=strlen(s+offs)+1;
    }
    return -1;
  }

  int getnum() {
    char *s=(char*)gr.get();
    int ml=gr.getlen();
    int offs=0;
    int idx=0;
    while (offs < ml)
    {
      offs+=strlen(s+offs)+1;
      idx++;
    }
    return idx;
  }

  char *get() { return (char*)gr.get(); }
  int getlen() { return gr.getlen(); }
private:
  GrowBuf gr;
};

template <class T>
class SortedStringList
{
  public:
    SortedStringList() { }
    ~SortedStringList()
    {
      T *s=(T*)gr.get();
      int num=gr.getlen()/sizeof(T);

      for (int i=0; i<num; i++) {
        free(s[i].name);
      }
    }

    // returns -1 when name already exists and pos if added
    int add(const char *name, int case_sensitive=0)
    {
      T newstruct={0,};
      int pos=find(name,case_sensitive,1);
      if (pos==-1) return -1;
      newstruct.name=(char*)malloc(strlen(name)+1);
      if (!newstruct.name)
      {
        extern FILE *g_output;
        extern int g_display_errors;
        extern void quit();
        if (g_display_errors) 
        {
          fprintf(g_output,"\nInternal compiler error #12345: GrowBuf realloc/malloc(%d) failed.\n",strlen(name)+1);
          fflush(g_output);
        }
        quit();
      }
      strcpy(newstruct.name,name);
      gr.add(&newstruct,sizeof(T));

      T *s=(T*)gr.get();
      memmove(s+pos+1,s+pos,gr.getlen()-((pos+1)*sizeof(T)));
      memcpy(s+pos,&newstruct,sizeof(T));

      return pos;
    }

    // returns -1 if not found, position if found
    // if returnbestpos=1 returns -1 if found, best pos to insert if not found
    int find(const char *str, int case_sensitive=0, int returnbestpos=0)
    {
      T *data=(T *)gr.get();
      int ul=gr.getlen()/sizeof(T);
      int ll=0;
      int nextpos=(ul+ll)/2;

      while (ul > ll)
      {
        int res;
        if (case_sensitive)
          res=strcmp(str, data[nextpos].name);
        else
          res=stricmp(str, data[nextpos].name);
        if (res==0) return returnbestpos ? -1 : nextpos;
        if (res<0) ul=nextpos;
        else ll=nextpos+1;
        nextpos=(ul+ll)/2;
      }
      
      return returnbestpos ? nextpos : -1;
    }

    // returns 0 on success, 1 otherwise
    int del(const char *str, int case_sensitive=0)
    {
      int pos=find(str, case_sensitive);
      if (pos==-1) return 1;

      T *db=(T *)gr.get();
      free(db[pos].name);
      freestruct(pos);
      memmove(db+pos,db+pos+1,gr.getlen()-(pos*sizeof(T))-sizeof(T));
      gr.resize(gr.getlen()-sizeof(T));

      return 0;
    }

    void delbypos(int pos)
    {
      T *db=(T *)gr.get();
      free(db[pos].name);
      memmove(db+pos,db+pos+1,gr.getlen()-(pos*sizeof(T))-sizeof(T));
      gr.resize(gr.getlen()-sizeof(T));
    }

  protected:
    TinyGrowBuf gr;
};

template <class T>
class SortedStringListND // no delete - can be placed in GrowBuf
{
  public:
    SortedStringListND() { }
    ~SortedStringListND() { }

    // returns -1 when name already exists and pos if added
    int add(const char *name, int case_sensitive=0, int alwaysreturnpos=0)
    {
      int where;
      T newstruct={0,};
      int pos=find(name,-1,case_sensitive,1,&where);
      if (pos==-1) return alwaysreturnpos ? where : -1;
      newstruct.name=strings.add(name,strlen(name)+1);
      
      gr.add(&newstruct,sizeof(T));
      T *s=(T*)gr.get();
      memmove(s+pos+1,s+pos,gr.getlen()-((pos+1)*sizeof(T)));
      memcpy(s+pos,&newstruct,sizeof(T));

      return pos;
    }

    // returns -1 if not found, position if found
    // if returnbestpos=1 returns -1 if found, best pos to insert if not found
    // if n_chars equal to -1 all string is tested
    int find(const char *str, size_t n_chars=-1, int case_sensitive=0, int returnbestpos=0, int *where=0)
    {
      T *data=(T *)gr.get();
      int ul=gr.getlen()/sizeof(T);
      int ll=0;
      int nextpos=(ul+ll)/2;

      while (ul > ll)
      {
        int res;
        const char *pCurr = (char*)strings.get() + data[nextpos].name;
        if (n_chars == -1 )
        {
          if (case_sensitive)
            res=strcmp(str, pCurr);
          else
            res=stricmp(str, pCurr);
        }
        else
        {
          if (case_sensitive)
            res=strncmp(str, pCurr, min(n_chars, strlen(pCurr)));
          else
            res=strnicmp(str, pCurr, min(n_chars, strlen(pCurr)));
          if ( res == 0 && n_chars != -1 && n_chars != strlen(pCurr) )
            res = n_chars - strlen(pCurr);
        }

        if (res==0)
        {
          if (where) *where = nextpos;
          return returnbestpos ? (case_sensitive!=-1 ? -1 : nextpos) : nextpos;
        }
        if (res<0) ul=nextpos;
        else ll=nextpos+1;
        nextpos=(ul+ll)/2;
      }
      
      return returnbestpos ? nextpos : -1;
    }

  protected:
    TinyGrowBuf gr;
    GrowBuf strings;
};

struct define {
  char *name;
  char *value;
};

class DefineList : public SortedStringList<struct define>
{
  public:
    DefineList() { }
    ~DefineList()
    {
      struct define *s=(struct define*)gr.get();
      int num=gr.getlen()/sizeof(struct define);

      for (int i=0; i<num; i++) {
        free(s[i].value);
      }
    }

    int add(const char *name, const char *value="")
    {
      int pos=SortedStringList<struct define>::add(name);
      if (pos == -1)
      {
        return 1;
      }

      char **newvalue=&(((struct define*)gr.get())[pos].value);
      *newvalue=(char*)malloc(strlen(value)+1);
      if (!(*newvalue))
      {
        extern FILE *g_output;
        extern int g_display_errors;
        extern void quit();
        if (g_display_errors) 
        {
          fprintf(g_output,"\nInternal compiler error #12345: GrowBuf realloc/malloc(%d) failed.\n",strlen(value)+1);
          fflush(g_output);
        }
        quit();
      }
      strcpy(*newvalue,value);
      return 0;
    }

    char *find(const char *name)
    {
      int v=SortedStringList<struct define>::find(name);
      if (v==-1)
      {
        return NULL;
      }
      return ((struct define*)gr.get())[v].value;
    }

    // returns 0 on success, 1 otherwise
    int del(const char *str)
    {
      int pos=SortedStringList<struct define>::find(str);
      if (pos==-1) return 1;

      struct define *db=(struct define *)gr.get();
      free(db[pos].value);
      delbypos(pos);

      return 0;
    }

    int getnum()
    {
      return gr.getlen()/sizeof(define);
    }

    char *getname(int num)
    {
      if ((unsigned int)getnum() <= (unsigned int)num)
        return 0;
      return ((struct define*)gr.get())[num].name;
    }

    char *getvalue(int num)
    {
      if ((unsigned int)getnum() <= (unsigned int)num)
        return 0;
      return ((struct define*)gr.get())[num].value;
    }
};

struct string_t {
  int name;
};

class FastStringList : public SortedStringListND<struct string_t>
{
  public:
    FastStringList() { }
    ~FastStringList() { }

    int add(const char *name, int case_sensitive=0)
    {
      int pos = SortedStringListND<struct string_t>::add(name, case_sensitive);
      if (pos == -1) return -1;
      return ((struct string_t*)gr.get())[pos].name;
    }

    char *get()
    {
      return (char*)strings.get();
    }

    int getlen()
    {
      return strings.getlen();
    }

    int getnum()
    {
      return gr.getlen()/sizeof(struct string_t);
    }
};

class MMapBuf : public IGrowBuf
{
  public:
    MMapBuf() 
    { 
      m_hFile = INVALID_HANDLE_VALUE;
      m_hFileMap = 0;
      m_mapping=NULL;
      m_gb_u=0;
      m_alloc=m_used=0;
    }
    ~MMapBuf() 
    { 
      if (m_mapping) UnmapViewOfFile(m_mapping);
      if (m_hFileMap) CloseHandle(m_hFileMap);
      if (m_hFile != INVALID_HANDLE_VALUE) CloseHandle(m_hFile);
    }

    int add(const void *data, int len) 
    { 
      if (len<=0) return 0;
      resize(getlen()+len); 
      memcpy((char*)get()+getlen()-len,data,len);
      return getlen()-len;
    }

    void resize(int newlen)
    {
      if (!m_gb_u && newlen < (16<<20)) // still in db mode
      {
        m_gb.resize(newlen);
        return;
      }
      m_gb_u=1;
      m_used=newlen;
      if (newlen > m_alloc)
      {
        if (m_mapping) UnmapViewOfFile(m_mapping);
        if (m_hFileMap) CloseHandle(m_hFileMap);
        m_hFileMap=0;
        m_mapping=NULL;
        m_alloc = newlen + (16<<20); // add 16mb to top of mapping
        if (m_hFile == INVALID_HANDLE_VALUE)
        {
          char buf[MAX_PATH],buf2[MAX_PATH];
          GetTempPath(MAX_PATH,buf);
          GetTempFileName(buf,"nsd",0,buf2);
          m_hFile=CreateFile(buf2,GENERIC_READ|GENERIC_WRITE,0,NULL,CREATE_ALWAYS,FILE_ATTRIBUTE_TEMPORARY|FILE_FLAG_DELETE_ON_CLOSE,NULL);
        }
        if (m_hFile != INVALID_HANDLE_VALUE)
          m_hFileMap=CreateFileMapping(m_hFile,NULL,PAGE_READWRITE,0,m_alloc,NULL);
        if (m_hFileMap) 
          m_mapping=MapViewOfFile(m_hFileMap,FILE_MAP_WRITE,0,0,m_alloc);
        if (!m_mapping)
        {
          extern FILE *g_output;
          extern void quit(); extern int g_display_errors;
          if (g_display_errors) 
          {
            fprintf(g_output,"\nInternal compiler error #12345: error mmapping datablock to %d.\n",m_alloc);
            fflush(g_output);
          }
          quit();
        }
        if (m_gb.getlen())
        {
          memcpy(m_mapping,m_gb.get(),m_gb.getlen());
          m_gb.resize(0);
        }
      }
    }

    int getlen() { if (m_gb_u) return m_used; return m_gb.getlen(); }
    void *get() { if (m_gb_u) return m_mapping; return m_gb.get(); }

  private:
    GrowBuf m_gb;
    int m_gb_u;

    HANDLE m_hFile, m_hFileMap;
    void *m_mapping;
    int m_alloc, m_used;
};


#endif//_STRLIST_H_
