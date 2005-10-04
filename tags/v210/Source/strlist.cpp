#include "strlist.h"

// ==========
// StringList
// ==========

int StringList::add(const char *str, int case_sensitive)
{
  int a=find(str,case_sensitive);
  if (a >= 0 && case_sensitive!=-1) return a;
  return gr.add(str,strlen(str)+1);
}

// use 2 for case sensitive end-of-string matches too
int StringList::find(const char *str, int case_sensitive, int *idx/*=NULL*/) const // returns -1 if not found
{
  const char *s=get();
  int ml=getlen();
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

void StringList::delbypos(int pos)
{
  char *s=(char*)gr.get();
  int len=strlen(s+pos)+1;
  if (pos+len < gr.getlen()) memcpy(s+pos,s+pos+len,gr.getlen()-(pos+len));
  gr.resize(gr.getlen()-len);
}

int StringList::idx2pos(int idx) const
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

int StringList::getnum() const
{
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

const char *StringList::get() const
{
  return (const char*)gr.get();
}

int StringList::getlen() const
{
  return gr.getlen();
}

// ==========
// DefineList
// ==========

DefineList::~DefineList()
{
  struct define *s=(struct define*)gr.get();
  int num=gr.getlen()/sizeof(struct define);

  for (int i=0; i<num; i++) {
    free(s[i].value);
  }
}

int DefineList::add(const char *name, const char *value/*=""*/)
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

char *DefineList::find(const char *name)
{
  int v=SortedStringList<struct define>::find(name);
  if (v==-1)
  {
    return NULL;
  }
  return ((struct define*)gr.get())[v].value;
}

// returns 0 on success, 1 otherwise
int DefineList::del(const char *str)
{
  int pos=SortedStringList<struct define>::find(str);
  if (pos==-1) return 1;

  struct define *db=(struct define *)gr.get();
  free(db[pos].value);
  delbypos(pos);

  return 0;
}

int DefineList::getnum()
{
  return gr.getlen()/sizeof(define);
}

char *DefineList::getname(int num)
{
  if ((unsigned int)getnum() <= (unsigned int)num)
    return 0;
  return ((struct define*)gr.get())[num].name;
}

char *DefineList::getvalue(int num)
{
  if ((unsigned int)getnum() <= (unsigned int)num)
    return 0;
  return ((struct define*)gr.get())[num].value;
}

// ==============
// FastStringList
// ==============

int FastStringList::add(const char *name, int case_sensitive/*=0*/)
{
  int pos = SortedStringListND<struct string_t>::add(name, case_sensitive);
  if (pos == -1) return -1;
  return ((struct string_t*)gr.get())[pos].name;
}

char *FastStringList::get() const
{
  return (char*)strings.get();
}

int FastStringList::getlen() const
{
  return strings.getlen();
}

int FastStringList::getnum() const
{
  return gr.getlen()/sizeof(struct string_t);
}

