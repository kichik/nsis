// Unicode support by Jim Park -- 08/22/2007

#include <windows.h>
#include "MyMath.h"
#include "Math.h"

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
            _RPT2(_CRT_WARN, _T("Memory leak %d at %8X\n"), i, blocks[i]);
        }
}

#endif

TCHAR *AllocString()
{
    return (TCHAR*) dbgGlobalAlloc(GPTR,g_stringsize*sizeof(TCHAR));
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
        lstrcpy((LPTSTR) item->param1, (LPTSTR) citem->param1);
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
