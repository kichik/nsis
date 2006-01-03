#include "strlist.h"

// ==========
// StringList
// ==========

int StringList::add(const TCHAR *str, int case_sensitive)
{
  int a=StringList::find(str,case_sensitive);
  if (a >= 0 && case_sensitive!=-1) 
		return a;
  return gr.add(str,sizeof(TCHAR)*(_tcslen(str)+1));
}

// use 2 for case sensitive end-of-string matches too
int StringList::find(const TCHAR *str, int case_sensitive, int *idx/*=NULL*/) const // returns -1 if not found
{
  const TCHAR *s=get();
  int ml=getlen();
  int offs=0;
  if (idx) *idx=0;
  while (offs < ml)
  {
    if ((case_sensitive && !_tcscmp(s+offs,str)) ||
        (!case_sensitive && !_tcsicmp(s+offs,str)))
    {
      return offs;
    }
    if (case_sensitive==2 &&
        _tcslen(str) < _tcslen(s+offs) &&  // check for end of string
        !_tcscmp(s+offs+_tcslen(s+offs)-_tcslen(str),str))
    {
      return offs+_tcslen(s+offs)-_tcslen(str);
    }
    offs+=_tcslen(s+offs)+1;

    if (idx) 
			*idx++;
  }
  return -1;
}

void StringList::delbypos(int pos)
{
  TCHAR *s=(TCHAR *)gr.get();
  int len=(_tcslen(s+pos)+1)*sizeof(TCHAR);
  if (pos+len < gr.getlen()) 
		memcpy(s+pos,s+pos+len,gr.getlen()-(pos+len));
  gr.resize(gr.getlen()-len);
}


int StringList::idx2pos(int idx) const
{
  TCHAR *s=(TCHAR *)gr.get();
  int offs=0;
  int cnt=0;
  if (idx>=0) while (offs < gr.getlen()*sizeof(TCHAR))
  {
    if (cnt++ == idx)
			return offs;
    offs+=_tcslen(s+offs)+1;
  }
  return -1;
}

int StringList::getnum() const
{
  TCHAR *s=(TCHAR *)gr.get();
  int ml=gr.getlen() / sizeof(TCHAR);
  int offs=0;
  int idx=0;
  while (offs < ml)
  {
    offs+=_tcslen(s+offs)+1;
    idx++;
  }
  return idx;
}

const TCHAR *StringList::get() const
{
  return (const TCHAR *)gr.get();
}

// TODO: review - is this supposed to be bytes or characters?
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

int DefineList::add(const TCHAR *name, const TCHAR *value/*=""*/)
{
  int pos=SortedStringList<struct define>::add(name);
  if (pos == -1)
  {
    return 1;
  }

  TCHAR **newvalue=&(((struct define*)gr.get())[pos].value);
  *newvalue=(TCHAR *)malloc((_tcslen(value)+1)*sizeof(TCHAR));
  if (!(*newvalue))
  {
    extern FILE *g_output;
    extern int g_display_errors;
    extern void quit();
    if (g_display_errors)
    {
      fprintf(g_output,"\nInternal compiler error #12345: GrowBuf realloc/malloc(%d) failed.\n",_tcslen(value)+1);
      fflush(g_output);
    }
    quit();
  }
  _tcscpy(*newvalue,value);
  return 0;
}

TCHAR *DefineList::find(const TCHAR *name)
{
  int v=SortedStringList<struct define>::find(name);
  if (v==-1)
  {
    return NULL;
  }
  return ((struct define*)gr.get())[v].value;
}

// returns 0 on success, 1 otherwise
int DefineList::del(const TCHAR *str)
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

TCHAR *DefineList::getname(int num)
{
  if ((unsigned int)getnum() <= (unsigned int)num)
    return 0;
  return ((struct define*)gr.get())[num].name;
}

TCHAR *DefineList::getvalue(int num)
{
  if ((unsigned int)getnum() <= (unsigned int)num)
    return 0;
  return ((struct define*)gr.get())[num].value;
}

// ==============
// FastStringList
// ==============

int FastStringList::add(const TCHAR *name, int case_sensitive/*=0*/)
{
  int pos = SortedStringListND<struct string_t>::add(name, case_sensitive);
  if (pos == -1) return -1;
  return ((struct string_t*)gr.get())[pos].name;
}

TCHAR *FastStringList::get() const
{
  return (TCHAR *)strings.get();
}

int FastStringList::getlen() const
{
  return strings.getlen();
}

int FastStringList::getnum() const
{
  return gr.getlen()/sizeof(struct string_t);
}
