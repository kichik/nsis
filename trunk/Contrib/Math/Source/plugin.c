#include <windows.h>
#include "MyMath.h"
#include "Math.h"

unsigned int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

#ifdef _DEBUG_LEAKS

#include <crtdbg.h>

int blocksnum = 0;
HGLOBAL blocks[100000];

HGLOBAL watchGlobalAlloc(UINT Flags, UINT size)
{
    HGLOBAL block = GlobalAlloc(Flags, size);
    blocks[blocksnum++] = block;
    return block;
}

void watchGlobalFree(HGLOBAL block)
{
    for (int i = 0; i < blocksnum; i++)
        if (blocks[i] == block) blocks[i] = NULL;
    GlobalFree(block);
}

void watchGlobal()
{
    for (int i = 0; i < blocksnum; i++)
        if (blocks[i] != NULL)
        {
            _RPT2(_CRT_WARN, "Memory leak %d at %8X\n", i, blocks[i]);
        }
}

#endif

// utility functions (not required but often useful)
int popstring(char *str)
{
  stack_t *th;
  if (!g_stacktop || !*g_stacktop) return 1;
  th=(*g_stacktop);
  lstrcpy(str,th->text);
  *g_stacktop = th->next;
  dbgGlobalFree((HGLOBAL)th);
  return 0;
}

void pushstring(char *str)
{
  stack_t *th;
  if (!g_stacktop) return;
  th=(stack_t*)dbgGlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize);
  lstrcpyn(th->text,str,g_stringsize);
  th->next=*g_stacktop;
  *g_stacktop=th;
}

char *getuservariable(int varnum)
{
  if (varnum < 0 || varnum >= __INST_LAST) return NULL;
  return g_variables+varnum*g_stringsize;
}

void setuservariable(int varnum, char *var)
{
    if (var != NULL && varnum >= 0 && varnum < __INST_LAST)
    lstrcpy(g_variables + varnum*g_stringsize, var);
}

char *AllocString()
{
    return (char*) dbgGlobalAlloc(GPTR,g_stringsize);
}

ExpressionItem *AllocItem()
{
    ExpressionItem *item = (ExpressionItem*)dbgGlobalAlloc(GPTR,sizeof(ExpressionItem));
    item->next = NULL;
    item->type = IT_CONST | ITC_INT;
    item->param1 = item->param2 = 0;
    return item;
}

ExpressionItem *AllocArray(int s)
{
    int size = DEFAULT_ARRAY_SIZE;
    while (s > size) size*=2;

    ExpressionItem *ai = (ExpressionItem*)dbgGlobalAlloc(GPTR,sizeof(ExpressionItem));
    ai->type = IT_CONST | ITC_ARRAY;
    ai->param1 = (EIPARAM) dbgGlobalAlloc(GPTR, sizeof(ArrayDesc));

    ArrayDesc *ad = *((ArrayDesc**)&(ai->param1));
    // initialize and clear the array memory
    ad->array = (ExpressionItem**) dbgGlobalAlloc(GPTR, size*sizeof(ExpressionItem*));
    ad->size = size;
    ad->count = 0;
    ad->references = 1;
    return ai;
}

ExpressionItem *CopyItem(ExpressionItem *citem, int NeedConst)
{
    if (!citem) return NULL;
    ExpressionItem *item = NULL;
    if ((NeedConst) && ((citem->type & ITEMTYPE) != IT_CONST))
    {
        // in case of non constant expression - flat it to const
        RunTree(citem, item, RTO_NEEDCONST | ITC_INT | ITC_STRING | ITC_FLOAT | ITC_ARRAY);
        if (item) return item;
    }

    item = AllocItem();
    item->type = citem->type;
    if ((item->type & (ITEMTYPE | ITEMSUBTYPE)) == (IT_CONST | ITC_STRING))
    {
        item->param1 = (EIPARAM) AllocString();
        lstrcpy((LPSTR) item->param1, (LPSTR) citem->param1);
    } else if (((item->type & (ITEMTYPE | ITEMSUBTYPE)) == (IT_CONST | ITC_ARRAY))
        ||
        ((item->type & (ITEMTYPE | ITEMSUBTYPE)) == (IT_VARIABLE | ITV_ARRITEM)))
    {
        item->param1 = citem->param1;
        ArrayDesc *ad = (ArrayDesc*) item->param1;
        ad->references++;
    }
    else item->param1 = citem->param1;
    item->param2 = citem->param2;
    item->next = NULL;
    return item;
}
