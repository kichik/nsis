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
    GrowBuf() { m_alloc=m_used=0; m_s=NULL; }
    ~GrowBuf() { free(m_s); }

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
      m_used=newlen;
      if (newlen > m_alloc)
      {
        void *n;
        m_alloc = newlen*2 + 32768;
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
      }
      if (!m_used && m_alloc > 65535) // only free if you resize to 0 and we're > 64k
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

  char *get() { return (char*)gr.get(); }
  int getlen() { return gr.getlen(); }
private:
  GrowBuf gr;
};


class DefineList
{
public:
  DefineList() { }
  ~DefineList() { }

  int add(const char *str, const char *value="")
  {
    if (defines.find(str,0)>=0) return 1;

    defines.add(str,0);
    values.add(value,-1);
    return 0;
  }

  int del(const char *str)
  {
    int id;
    int v=defines.find(str,0,&id);
    if (v<0) return 1;
    id=values.idx2pos(id);
    if (id<0)return 1;
    defines.delbypos(v);
    values.delbypos(id);
    return 0;
  }

  char *find(const char *str) // returns NULL if not found
  {
    int id;
    int v=defines.find(str,0,&id);
    if (v<0) return NULL;
    v=values.idx2pos(id);
    if (v<0) return NULL;
    return (char*)values.get()+v;
  }

  StringList defines, values;
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
