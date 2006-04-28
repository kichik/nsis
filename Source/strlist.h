#ifndef _STRLIST_H_
#define _STRLIST_H_

#include "Platform.h"
#include <cstdio>
#include "growbuf.h"

class StringList
{
private: // don't copy instances
  StringList(const StringList&);
  void operator=(const StringList&);

public:
  StringList() {}
  ~StringList() {}

  int add(const char *str, int case_sensitive);
  // use 2 for case sensitive end-of-string matches too
  int find(const char *str, int case_sensitive, int *idx=NULL) const; // returns -1 if not found
  void delbypos(int pos);
  int idx2pos(int idx) const;
  int getnum() const;
  const char *get() const;
  int getlen() const;

private:
  GrowBuf gr;
};

template <class T>
class SortedStringList
{
  public:
    virtual ~SortedStringList()
    {
      T *s=(T*)gr.get();
      size_t num=gr.getlen()/sizeof(T);

      for (size_t i=0; i<num; i++) {
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

      delbypos(pos);

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

#define mymin(x, y) ((x < y) ? x : y)

template <class T>
class SortedStringListND // no delete - can be placed in GrowBuf
{
  public:
    SortedStringListND() { }
    virtual ~SortedStringListND() { }

    // returns -1 when name already exists and pos if added
    int add(const char *name, int case_sensitive=0, int alwaysreturnpos=0)
    {
      int where=0;
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
    int find(const char *str, int n_chars=-1, int case_sensitive=0, int returnbestpos=0, int *where=0)
    {
      T *data=(T *)gr.get();
      int ul=gr.getlen()/sizeof(T);
      int ll=0;
      int nextpos=(ul+ll)/2;

      while (ul > ll)
      {
        int res;
        const char *pCurr = (char*)strings.get() + data[nextpos].name;
        if (n_chars < 0)
        {
          if (case_sensitive)
            res = strcmp(str, pCurr);
          else
            res = stricmp(str, pCurr);
        }
        else
        {
          if (case_sensitive)
            res=strncmp(str, pCurr, mymin((unsigned int) n_chars, strlen(pCurr)));
          else
            res=strnicmp(str, pCurr, mymin((unsigned int) n_chars, strlen(pCurr)));
          if (res == 0 && n_chars != -1 && (unsigned int) n_chars != strlen(pCurr))
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
  private: // don't copy instances
    DefineList(const DefineList&);
    void operator=(const DefineList&);

  public:
    DefineList() {} // VC6 complains otherwise
    virtual ~DefineList();

    int add(const char *name, const char *value="");
    char *find(const char *name);

    // returns 0 on success, 1 otherwise
    int del(const char *str);
    int getnum();
    char *getname(int num);
    char *getvalue(int num);
};

struct string_t {
  int name;
};

class FastStringList : public SortedStringListND<struct string_t>
{
  private: // don't copy instances
    FastStringList(const FastStringList&);
    void operator=(const FastStringList&);
    
  public:
    FastStringList() {} // VC6 complains otherwise
    virtual ~FastStringList() {}

    int add(const char *name, int case_sensitive=0);
    char *get() const;
    int getlen() const;
    int getnum() const;
};

#endif//_STRLIST_H_
