#include <windows.h>
#include "MyMath.h"
#include "Math.h"

extern "C" int _fltused;
ExpressionItem *stack;

int UserVarsCount, UserFuncsCount;
UserVar UserVars[MAX_USER_VARS];
UserFunc UserFuncs[MAX_USER_FUNCS];

void PrintTree(ExpressionItem *root, char *str);
void ParseString(char *&sp, ExpressionItem* &itemplace);
void CleanupItems(ExpressionItem* &itemplace);
void PlaceVariable(char *&vb, ParseInfo *pi);

void PlaceNewItem(char *&vb, ParseInfo *pi, int precedence)
{
    ExpressionItem *newroot;
    PlaceVariable(vb, pi);
    if (pi->item == NULL) return;

    while ((pi->OpsStack) && ((((int) pi->OpsStack->param2) < precedence)
        || ((((int)pi->OpsStack->param2) == precedence)
            && (precedence != OPERATOR_SET_PRECEDENCE))))
    {
        // second operand for our operator
        newroot = pi->OpsStack;
        *((ExpressionItem **)&(newroot->param2)) = pi->item;
        pi->OpsStack = newroot->next;
        newroot->next = NULL;
        pi->item = newroot;
    }
    // finally we have got new root
    newroot = pi->item;

    if (pi->SetupNewRoot)
    {
        (*pi->root)->next = newroot;
        pi->root = &((*pi->root)->next);
        pi->SetupNewRoot = 0;
    }
    if (*pi->place == *pi->root) *pi->place = *pi->root = newroot;
    else *pi->root = newroot;
    // no item at our pockets
    pi->item = NULL;
}

#define NSIS_VARS_COUNT 27
#define NSIS_VARS_STACK 25
#define NSIS_VARS_NSTACK 26

typedef char smallstr[2];
const smallstr NSISVariablesNames[NSIS_VARS_COUNT] = {{'r','0'}, {'r','1'}, {'r','2'}, {'r','3'}, {'r','4'}, {'r','5'}, {'r','6'}, {'r','7'}, {'r','8'}, {'r','9'},
{'R','0'}, {'R','1'}, {'R','2'}, {'R','3'}, {'R','4'}, {'R','5'}, {'R','6'}, {'R','7'}, {'R','8'}, {'R','9'},
{'C','L'}, {'I','D'}, {'O','D'}, {'E','D'}, {'L','G'}, {'S',0}, {'N','S'}};

ExpressionItem *FindVariable(char *varname)
{
    int i;

    ExpressionItem *item = AllocItem();
    // check NSIS variables
    for (i = 0; i < NSIS_VARS_COUNT; i++)
    {
        if (lstrcmpn(varname, NSISVariablesNames[i],2) == 0)
        {
            if (i == NSIS_VARS_STACK) item->type = IT_VARIABLE | ITV_STACK;
            else if (i == NSIS_VARS_NSTACK) item->type = IT_VARIABLE | ITV_NSTACK;
            else
                item->type = (IT_VARIABLE | ITV_NSIS) + i;
            return item;
        }
    }
    // no.. that's user variable
    for (i = 0; i < UserVarsCount; i++)
    {
        if (lstrcmp(varname, UserVars[i].name) == 0)
        {
            // ok. we found user var expression needed
            break;
        }
    }
    if (i == UserVarsCount)
    {
        // new variable
        UserVarsCount++;
        lstrcpy(UserVars[i].name, varname);
        UserVars[i].item = NULL;
    }
    item->type = (IT_VARIABLE | ITV_USER) + i;
    return item;
}

void PlaceVariable(char *&vb, ParseInfo *pi)
{
    if (vb <= pi->valbuf) return;
    *vb = 0;
    pi->item = FindVariable(pi->valbuf);
    vb = pi->valbuf;
}

typedef double (*math_d2)(double, double);
typedef double (*math_ddp)(double, double*);
typedef double (*math_di)(double, int*);

#define MATHFUNCNUM 29
const MathFunction MathFunctions[MATHFUNCNUM] = {
    {{'s','i','n'}, ITF_MATH1 >> 8, sin},
    {{'s','n','h'}, ITF_MATH1 >> 8, sinh},
    {{'a','s','n'}, ITF_MATH1 >> 8, asin},
    {{'c','o','s'}, ITF_MATH1 >> 8, cos},
    {{'c','s','h'}, ITF_MATH1 >> 8, cosh},
    {{'a','c','s'}, ITF_MATH1 >> 8, acos},
    {{'t','a','n'}, ITF_MATH1 >> 8, tan},
    {{'t','n','h'}, ITF_MATH1 >> 8, tanh},
    {{'a','t','n'}, ITF_MATH1 >> 8, atan},
    {{'a','b','s'}, ITF_MATH1 >> 8, fabs},
    {{'l','n',0}, ITF_MATH1 >> 8, log},
    {{'l','o','g'}, ITF_MATH1 >> 8, log10},
    {{'e','x','p'}, ITF_MATH1 >> 8, exp},
    {{'s','q','t'}, ITF_MATH1 >> 8, sqrt},
    {{'c','e','l'}, ITF_MATH1 >> 8, ceil},
    {{'f','l','r'}, ITF_MATH1 >> 8, floor},

    {{'a','t','2'}, ITF_MATH2 >> 8, (Math1FuncPtr)(math_d2)atan2},
    {{'p','o','w'}, ITF_MATH2 >> 8, (Math1FuncPtr)(math_d2)pow},
    {{'f','m','d'}, ITF_MATH2 >> 8, (Math1FuncPtr)(math_d2)fmod},

    // type conversions
    {{'i',0,0}, ITF_TYPE >> 8, (Math1FuncPtr)ITC_INT},
    {{'s',0,0}, ITF_TYPE >> 8, (Math1FuncPtr)ITC_STRING},
    {{'f',0,0}, ITF_TYPE >> 8, (Math1FuncPtr)ITC_FLOAT},
    {{'a',0,0}, ITF_TYPE >> 8, (Math1FuncPtr)ITC_ARRAY},
#define ITFT_CARRAY_ID    23
    {{'c','a',0}, ITF_TYPE >> 8, (Math1FuncPtr)ITC_ARRAY},
    {{'f','f',0}, ITF_TYPE >> 8, (Math1FuncPtr)FTT_FLOATF},
    {{'l',0,0}, ITF_TYPE >> 8, (Math1FuncPtr)FTT_LEN},
    {{'c',0,0}, ITF_TYPE >> 8, (Math1FuncPtr)FTT_CHAR},

    {{'f','e','x'}, ITF_MATH2 >> 8, (Math1FuncPtr)(math_di)frexp},
    {{'m','d','f'}, ITF_MATH2 >> 8, (Math1FuncPtr)(math_ddp)modf},
};

void PlaceFunction(char *&vb, char *&sp, ParseInfo *pi, int redefine)
{
    int i;
    ExpressionItem *item = pi->item = AllocItem();
    *vb = 0;

    // check BUILTIN functions
    for (i = 0; i < MATHFUNCNUM; i++)
    {
        if (lstrcmpn(pi->valbuf, MathFunctions[i].name, 3) == 0)
        {
            item->type = IT_FUNCTION | (MathFunctions[i].type << 8) | i;
            // get first argument
            sp++;
            ParseString(sp, *((ExpressionItem **)(&item->param1)));
            if (*sp == ',')
            {
                // get second argument
                sp++;
                ParseString(sp, *((ExpressionItem **)(&item->param2)));
            }
            sp++; vb = pi->valbuf;
            return;
        }
    }

    // heh, may be it user function
    for (i = 0; i < UserFuncsCount; i++)
    {
        if (lstrcmp(pi->valbuf, UserFuncs[i].name) == 0)
        {
            // Function found? Redefine option specified?
            if (redefine) break;

            item->type = IT_FUNCTION | ITF_USER | i;
            // get arguments list
            ExpressionItem **newplace = ((ExpressionItem **)(&pi->item->param1));
            while (*sp != ')')
            {
                *newplace = AllocItem();
                (*newplace)->type = IT_EXPRESSION;
                sp++;
                ParseString(sp, *((ExpressionItem **)(&(*newplace)->param1)));
                newplace = &((*newplace)->next);
            }
            sp++; vb = pi->valbuf;
            return;
        }
    }

    // oops, we need no item for function defenition
    CleanupItems(item); pi->item = NULL;

    // it's user function define
    int flags = 0;
    char buffer[128], *buf = buffer;

    // workaround for redefine flag - if the function already present,
    // it will be cleared and redefined
    UserFunc *f = &UserFuncs[i];
    if (i == UserFuncsCount) UserFuncsCount++;
    else CleanupItems(f->root);

    lstrcpy(f->name, pi->valbuf);
    f->varflags = 0;
    f->varsnum = 0;
    do
    {
        sp++;
        switch (*sp)
        {
        case ' ':
            break;
        case ',':
        case ')':
            if (buf > buffer)
            {
                *buf = 0;
                // it should be user variable
                ExpressionItem *it = FindVariable(buffer);
                f->vars[f->varsnum++] = (it->type) & ITEMOPTIONS;
                CleanupItems(it);
                buf = buffer;
                flags <<= 1;
            }
            break;
        case '&':
            flags |= 1;
            break;
        default:
            *(buf++) = *sp;
            break;
        }
    }
    while (*sp != ')');

    // prepare flag for fast analisys
    for (i = 0; i < f->varsnum; i++)
    {
        f->varflags <<= 1;
        flags >>= 1;
        f->varflags |= flags&1;
    }

    // find nearest round bracket - function body
    while (*sp != '(' && *sp) sp++;
    if (!*sp)
      return;
    sp++;

    // now we are ready to parse function body
    ParseString(sp, f->root);
    sp++; // closing bracket
    vb = pi->valbuf;

#ifdef _DEBUG
  // dump function (in debug mode)
  char place[1024];
  wsprintf(place, "function %s(", f->name);
  flags = f->varflags;
  for (i = 0; i < f->varsnum; i++)
  {
    if (flags&1) lstrcat(place, "&");
    lstrcat(place, UserVars[f->vars[i]].name);
    if (i < f->varsnum-1) lstrcat(place, ", ");
    flags >>= 1;
  }
  lstrcat(place, ")");
  PrintTree(f->root, place);
#endif
}

// operator options
#define PO_UNARYPRE    0x1 // this operator can be uniary pre (--a) for ex
#define PO_UNARYPOST   0x2 // this op can be uniary post (a++) (couldn't be binary)
#define PO_PRENONCONST  0x4 // pre argument (a = b) -> a is non const
#define PO_POSTNONCONST 0x8 // post argument (b--) is non const
#define PO_LASTOP      0x10 // op should be the last item at expression (=, -=, etc)
#define PO_SET         0x20 // op will set new value to one of args
#define PO_USESPRE     0x40 // operator will use pre operand
#define PO_USESPOST    0x80 // operator will use post operan

void PlaceOp(char *&vb, int type, int precedence, ParseInfo *pi)
{
    PlaceVariable(vb, pi);
    if ((type & PO_UNARYPRE) && (!pi->item))
    {
        // uniary pre op
        ExpressionItem *item = AllocItem();
        item->type = type;
        item->param2 = (EIPARAM) precedence;
        item->next = pi->OpsStack;
        pi->OpsStack = item;
    }
    else
    {
        // get previous tree as items and operators of lower precedence
        PlaceNewItem(vb, pi, precedence);
        // post operators
        ExpressionItem *item = AllocItem();
        item->type = type;
        item->param1 = (EIPARAM) (*pi->root);

        if (*pi->place == *pi->root) *pi->place = *pi->root = NULL;
        else *pi->root = NULL;

        if (type & PO_UNARYPOST)
        {
            // uniary post op
            pi->item = item;
        } else
        {
            // binary operator
            item->param2 = (EIPARAM) precedence;
            item->next = pi->OpsStack;
            pi->OpsStack = item;
        }
    }
}

#define OPSNUM  35
const OpStruct Operators[OPSNUM] =
{
// three byte ops
{{'>','>','='}, 14, ITO_SHR | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{{'<','<','='}, 14, ITO_SHL | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},

// two byte ops
// !!! don't forget to change Set Operator Precedence !!!
{"-=", 14, ITO_MINUS | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{"+=", 14, ITO_PLUS | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{"/=", 14, ITO_DIV | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{"*=", 14, ITO_MUL | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{"|=", 14, ITO_OR | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{"&=", 14, ITO_AND | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{"^=", 14, ITO_XOR | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{"%=", 14, ITO_MOD | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPRE | PO_USESPOST},
{"--", 2, ITO_DEC | PO_POSTNONCONST | PO_PRENONCONST | PO_UNARYPRE | PO_UNARYPOST | PO_SET | PO_USESPRE | PO_USESPOST},
{"++", 2, ITO_INC | PO_POSTNONCONST | PO_PRENONCONST | PO_UNARYPRE | PO_UNARYPOST | PO_SET | PO_USESPRE | PO_USESPOST},
{">>", 6, ITO_SHR | PO_USESPRE | PO_USESPOST},
{"<<", 6, ITO_SHL | PO_USESPRE | PO_USESPOST},

// logical
{"&&", 12, ITO_LAND | PO_USESPRE | PO_USESPOST},
{"||", 13, ITO_LOR | PO_USESPRE | PO_USESPOST},

// comparisons
{"<=", 7, ITO_LE | PO_USESPRE | PO_USESPOST},
{"=<", 7, ITO_LE | PO_USESPRE | PO_USESPOST},
{">=", 7, ITO_GE | PO_USESPRE | PO_USESPOST},
{"=>", 7, ITO_GE | PO_USESPRE | PO_USESPOST},
{"!=", 8, ITO_NE | PO_USESPRE | PO_USESPOST},
{"==", 8, ITO_EQ | PO_USESPRE | PO_USESPOST},

// single byte ops
// !!! don't forget to change Set Operator Precedence !!!
{"=", 14, ITO_SET | PO_PRENONCONST | PO_LASTOP | PO_SET | PO_USESPOST},
{"+", 5, ITO_PLUS | PO_USESPRE | PO_USESPOST},
{"-", 5, ITO_MINUS | PO_USESPRE | PO_USESPOST | PO_UNARYPRE},
{"*", 4, ITO_MUL | PO_USESPRE | PO_USESPOST | PO_UNARYPRE},
{"/", 4, ITO_DIV | PO_USESPRE | PO_USESPOST},
{"%", 4, ITO_MOD | PO_USESPRE | PO_USESPOST},
{"<", 7, ITO_LS | PO_USESPRE | PO_USESPOST},
{">", 7, ITO_GR | PO_USESPRE | PO_USESPOST},
{"&", 9, ITO_AND | PO_USESPRE | PO_USESPOST | PO_UNARYPRE},
{"|", 11, ITO_OR | PO_USESPRE | PO_USESPOST},
{"^", 10, ITO_XOR | PO_USESPRE | PO_USESPOST},
{"~", 3, ITO_NOT | PO_USESPOST | PO_UNARYPRE},
{"!", 3, ITO_LNOT |PO_USESPOST | PO_UNARYPRE}
};

void CheckForOperator(char *&sp, char *&vb, ParseInfo *pi)
{
    for (int op = 0; op < OPSNUM; op++)
    {
        int c = lstrlen(Operators[op].name);
        if (c > 3) c = 3; // real operator length
        if (lstrcmpn(sp, Operators[op].name, c))
        {
            // wrong - different op
            continue;
        }
        // that is our op
        sp += c;
        PlaceOp(vb, ((int) Operators[op].type) | IT_OPERATOR, Operators[op].precedence, pi);
        break;
    }
}

void ParseString(char *&sp, ExpressionItem* &itemplace)
{
    ParseInfo pi = {0, NULL, NULL, &itemplace, &itemplace};

    int redefine = 0;
    char *vb = pi.valbuf;
    // cycle until current expression end
    while ((*sp != 0) && (*sp != ')') && (*sp != '}') &&
        (*sp != ']') && (*sp != ','))
    {
        int processed = 1;
        switch (*sp)
        {
        case ' ':
        case '\t':
            sp++;
            break;
        case ';':
            // expression delimeter
            PlaceNewItem(vb, &pi, 255);
            if (*pi.root) pi.SetupNewRoot = 1;
            sp++;
            break;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            // variable & function names could contain numbers as non first chars
            if (vb > pi.valbuf)
            {
                processed = FALSE;
                break;
            }
        case '\'': case '\"': case '`':
            // constant meet
            pi.item = AllocItem();
            StringToItem(sp, pi.item, STI_STRING | STI_FLOAT | STI_INT);
            break;

        case '(': // start of function or expression
            if (vb > pi.valbuf)
            {
                // thats function
                PlaceFunction(vb, sp, &pi, redefine);
            } else
            {
                // expression
                sp++;
                ParseString(sp, pi.item);
                if (*sp == ')') sp++;
            }
            redefine = 0;
            break;

        case '#':   // start of one of logical expresions
            sp++;
            if ((*sp != '[') && (*sp != '{'))
            {
                // function redefine flag
                redefine = 1;
                break;
            }
            {
                pi.item = AllocItem();
                // IF or WHILE
                pi.item->type = ((*sp == '[')?(IT_LOGIC | ITL_IF):(IT_LOGIC | ITL_WHILE));
                // first expr - logic statement
                sp++;

                ParseString(sp, *((ExpressionItem **)(&pi.item->param1)));
                // ok, second expr - then, third - else statement.. others???
                ExpressionItem **newplace = ((ExpressionItem **)(&pi.item->param2));
                while (*sp == ',')
                {
                    *newplace = AllocItem();
                    (*newplace)->type = IT_EXPRESSION;
                    sp++;
                    ParseString(sp, *((ExpressionItem **)(&(*newplace)->param1)));
                    newplace = &((*newplace)->next);
                }
            }
            sp++;
            break;

        case '[':
            {
            // thats array access
            PlaceOp(vb, IT_ARRAY | ITA_ACCESS | PO_UNARYPOST, 1, &pi);
            sp++;
            // item index
            ParseString(sp, *(ExpressionItem **)&(pi.item->param2));
            if (*sp == ',')
            {
                // two indexes - string access
                ExpressionItem *it = AllocItem();
                it->type = IT_EXPRESSION;
                it->param1 = (EIPARAM)  *(ExpressionItem **)&(pi.item->param2);
                *(ExpressionItem **)&(pi.item->param2) = it;
                it = it->next = AllocItem();
                it->type = IT_EXPRESSION;
                sp++;
                ParseString(sp, *((ExpressionItem **)(&it->param1)));
            }
            sp++;
            }
            break;

        case '{':   // start of array define
            {
                // array define - constists of array copy operator and array define itself

                // type conversion item (to create a copy of array)
                pi.item = AllocItem();
                pi.item->type = IT_FUNCTION | ITF_TYPE | ITFT_CARRAY_ID | ITFA_COPY;

                // during first create our array descriptor and array pointers
                ExpressionItem *ai = AllocArray(DEFAULT_ARRAY_SIZE);
                pi.item->param1 = (EIPARAM)  ai;
                ArrayDesc *ad = *((ArrayDesc**)&(ai->param1));

                // parse array initializers
                while (*sp != '}')
                {
                    sp++;
                    ParseString(sp, ad->array[ad->count]);
                    if (ad->array[ad->count])
                      ad->count++;
                }

                sp++;
            }
            break;
        case '-': case '+': case '<': case '=': case '>':
        case '/': case '*': case '~': case '^': case '!':
        case '&': case '|': case '%':
            CheckForOperator(sp, vb, &pi);
            break;

        // non expression? ok, then it should be common char, like function or var name
        default:
            processed = FALSE;
            break;
        }
        if (!processed) *(vb++) = *(sp++);
    }
    PlaceNewItem(vb, &pi, 255);
}

void CleanupArray(ArrayDesc *ad)
{
    if (!(--(ad->references)))
    {
        // last array reference, we could kill it
        // cleanup array items
        for (int i = 0; i < ad->count; i++)
        {
            ExpressionItem *aritem = ad->array[i];
            if (aritem)
                CleanupItems(aritem);
        }
        // cleanup ptrs and descriptor
        dbgGlobalFree(ad->array);
        dbgGlobalFree(ad);
    }
 }

void CleanupItems(ExpressionItem* &itemplace)
{
    if (itemplace == NULL) return;
    ExpressionItem *item = itemplace, *itemnext;
    do
    {
        if (((item->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_VARIABLE|ITV_ARRITEM))
            ||
            ((item->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_CONST|ITC_ARRAY)))
        {
            CleanupArray((ArrayDesc *)item->param1);
        }
        else
        if ((item->type & ITEMTYPE) == IT_CONST)
        {
            if ((item->type & ITEMSUBTYPE) == ITC_STRING)
                dbgGlobalFree((HGLOBAL) item->param1);
        } else
        {
            CleanupItems(*((ExpressionItem**) &item->param1));
            CleanupItems(*((ExpressionItem**) &item->param2));
        }
        // free the item itself
        itemnext = item->next;
        dbgGlobalFree((HGLOBAL) item);
        item = itemnext;
    } while (item != NULL);
    itemplace = NULL;
}

#ifdef _DEBUG
HANDLE myfile;

char *opsnames[] = {"", "-", "+", "<<", ">>", "*", "/", "=", "&&", "||", "++", "--",
"=<", ">=", "!=", "==", "<", ">", "&", "%", "|", "^", "~", "!"};

void PrintNode(int index, int spaces, ExpressionItem* itemplace)
{
    if (itemplace == NULL) return;

    ExpressionItem *item = itemplace;
    do
    {
        DWORD wrote;
        char buffer[1024], *place = buffer;
        for (int k = 0; k < spaces; k++)
            *(place++) = 32;
        *place = 0;

        switch (item->type & ITEMTYPE)
        {
        case  IT_EXPRESSION:
            wsprintf(place, "Expression Place-Holder   ");
            break;
        case  IT_CONST:
            switch (item->type & ITEMSUBTYPE)
            {
            case ITC_STRING:
                wsprintf(place, "String: \"%s\"", (char *) item->param1);
                break;
            case ITC_INT:
                wsprintf(place, "Int: ");
                itoa64(*((__int64*)&(item->param1)), place+5);
                break;
            case ITC_FLOAT:
                wsprintf(place, "Float: ");
                FloatFormat(place+7, *((double*)&(item->param1)), 6);
                break;
            case ITC_ARRAY:
                ArrayDesc *ad = (ArrayDesc*) item->param1;
                wsprintf(place, "Array, ptr %08X, size %d, count %d, references %d",
                    ad->array, ad->size, ad->count, ad->references);
                break;
            }
            strcat(place, "    ");
            break;
        case IT_OPERATOR:
            wsprintf(place, "Op: %s%s    ", opsnames[(item->type & ITEMSUBTYPE) >> 8], (item->type & PO_SET)?("(=)"):(""));
            break;
        case IT_VARIABLE:
            switch (item->type & ITEMSUBTYPE)
            {
            case ITV_NSIS:
                {
                char buffer[128];
                buffer[0] = NSISVariablesNames[item->type & ITEMOPTIONS][0];
                buffer[1] = NSISVariablesNames[item->type & ITEMOPTIONS][1];
                buffer[2] = 0;
                wsprintf(place, "Var: %s (%d)   ",
                    buffer,
                    item->type & ITEMOPTIONS);
                }
                break;
            case ITV_USER:
                wsprintf(place, "Var: %s (%d)   ", UserVars[item->type & ITEMOPTIONS].name, item->type & ITEMOPTIONS);
                break;
            case ITV_STACK:
                wsprintf(place, "Plugin Stack   ");
                break;
            case ITV_NSTACK:
                wsprintf(place, "NSIS Stack   ");
                break;
            }
            break;
        case IT_LOGIC:
            if ((item->type & ITEMSUBTYPE) == ITL_IF)
                wsprintf(place, "IF ");
            else
                wsprintf(place, "WHILE ");
            break;
        case IT_FUNCTION:
            if (((item->type & ITEMSUBTYPE) == ITF_MATH1) ||
                ((item->type & ITEMSUBTYPE) == ITF_MATH2) ||
                ((item->type & ITEMSUBTYPE) == ITF_TYPE))
            {
                char buffer[128];
                buffer[0] = (MathFunctions[item->type &ITEMOPTIONS].name)[0];
                buffer[1] = (MathFunctions[item->type &ITEMOPTIONS].name)[1];
                buffer[2] = (MathFunctions[item->type &ITEMOPTIONS].name)[2];
                buffer[3] = 0;
                wsprintf(place, "Built-In Function %s() [%d] ",
                    buffer,
                    item->type &ITEMOPTIONS);
            }
            else
            {
                UserFunc *f = &(UserFuncs[item->type & ITEMOPTIONS]);
                wsprintf(place, "User Function: %s(", f->name);
                int flags = f->varflags;
                for (int i = 0; i < f->varsnum; i++)
                {
                    if (flags&1) lstrcat(place, "&");
                    lstrcat(place, UserVars[f->vars[i]].name);
                    if (i < f->varsnum-1) lstrcat(place, ", ");
                    flags >>= 1;
                }
                lstrcat(place, ")   ");
            }
            break;
        case IT_ARRAY:
            wsprintf(place, "Array access []   ");
            break;
        }
        place += lstrlen(place);
        wsprintf(place, "Addr: %08X Type: %08X   Next: %08X  Param1: %08X  Param2: %08X", item, item->type, item->next, item->param1, item->param2);
        lstrcat(place, "\n");
        WriteFile(myfile, buffer, lstrlen(buffer), &wrote, NULL);
        if (((item->type & ITEMTYPE) != IT_CONST) && ((item->type & (ITEMTYPE|ITEMSUBTYPE)) != (IT_VARIABLE|ITV_ARRITEM)))
        {
            place = buffer;
            for (int k = 0; k < spaces+2; k++)
                *(place++) = 32;
            int show = 0;
            if (((item->param1 != NULL) && ((*((ExpressionItem**) &item->param1))->next != NULL)) ||
                ((item->param2 != NULL) && ((*((ExpressionItem**) &item->param2))->next != NULL)))
                show = 1;
            if (show)
            {
                wsprintf(place, "Sub1:\n");
                WriteFile(myfile, buffer, lstrlen(buffer), &wrote, NULL);
            }
            PrintNode(1, spaces + 4, *((ExpressionItem**) &item->param1));
            if (show)
            {
                wsprintf(place, "Sub2:\n");
                WriteFile(myfile, buffer, lstrlen(buffer), &wrote, NULL);
            }
            PrintNode(2, spaces + 4, *((ExpressionItem**) &item->param2));
        } else if ((item->type & (ITEMSUBTYPE|ITEMTYPE)) == (ITC_ARRAY|IT_CONST))
        {
            ArrayDesc *ad = (ArrayDesc *) item->param1;
            for (int i = 0; i < ad->count; i++)
            {
                ExpressionItem *aritem = ad->array[i];
                if (aritem)
                    PrintNode(2, spaces + 4, aritem);
            }
        }
        item = item->next;
    } while (item != NULL);
}

void PrintTree(ExpressionItem *root, char *str)
{
    myfile = CreateFile("d:\\math.debug", GENERIC_ALL, FILE_SHARE_READ, NULL, OPEN_ALWAYS, 0, 0);
    SetFilePointer(myfile, 0, NULL, FILE_END);
    char buffer[1024]; DWORD wrote;
    wsprintf(buffer, "New tree for \'%s\'\n", str);
    WriteFile(myfile, buffer, lstrlen(buffer), &wrote, NULL);

    PrintNode(0, 4, root);
    CloseHandle(myfile);
    myfile = NULL;
}
#endif

void CopyArray(ExpressionItem *&item)
{
    if (item == NULL) return;
    // especial case - array to array conversion is requested array copy
    ExpressionItem *olditem = item;
    ArrayDesc *oad = (ArrayDesc *) (olditem->param1);
    // Initialize the array of the same size
    item = AllocArray(oad->size);
    ArrayDesc *nad = (ArrayDesc *) (item->param1);
    nad->count = oad->count;
    // copy items
    for (int i = 0; i < oad->count; i++)
       nad->array[i] = CopyItem(oad->array[i], TRUE);
    // cleanup old array pointer (may be array itself)
    CleanupItems(olditem);
}

void ItemToType(ExpressionItem* &item, int type)
{
    char *buffer, *bp;
    if (item == NULL) return;
    int itemt = item->type & ITEMTYPE, oldtype = item->type & ITEMSUBTYPE;

    if (((itemt == IT_CONST) && (oldtype == type))
        || (itemt != IT_CONST)) return;

    switch (type)
    {
    case ITC_STRING:
        buffer = AllocString();
        ItemToString(buffer, item);
        item->param1 = (EIPARAM)  buffer;
        item->param2 = 0;
        break;
    case ITC_FLOAT:
        if (oldtype == ITC_INT)
            *((double *)&(item->param1)) = (double) *((__int64 *)&(item->param1));
        else
        {
            bp = buffer = (char*) item->param1;
            StringToItem(buffer, item, STI_FLOAT);
            dbgGlobalFree(bp);
        }
        break;
    case ITC_INT:
        if (oldtype == ITC_FLOAT)
            *((__int64 *)&(item->param1)) = (__int64) *((double *)&(item->param1));
        else
        {
            bp = buffer = (char*) item->param1;
            StringToItem(buffer, item, STI_INT);
            dbgGlobalFree(bp);
        }
        break;
    case ITC_ARRAY:
        if (oldtype == ITC_STRING)
        {
            char *buf = (char*) item->param1;
            int len = lstrlen(buf)+1;
            ExpressionItem *ni = AllocArray(lstrlen(buf)+1);
            ArrayDesc *ad = (ArrayDesc*) ni->param1;
            for (int i = 0; i < len; i++)
            {
                ad->array[i] = AllocItem();
                *((__int64 *) &(ad->array[i]->param1)) = (__int64) buf[i];
            }
            ad->count = len;
            CleanupItems(item);
            item = ni;
        }
        break;
    }
    item->type = IT_CONST | type;
}

void SaveResult(ExpressionItem *var, ExpressionItem *result)
{
    if ((var->type & ITEMTYPE) != IT_VARIABLE) return;

    // result should be stored at variable to
    int varindex = var->type & ITEMOPTIONS;
    switch (var->type & ITEMSUBTYPE)
    {
    case ITV_NSIS:
        {
            // store string result direct to NSIS variable
            char *ptr = g_variables + varindex*g_stringsize;
            ItemToString(ptr, result);
        }
        break;
    case ITV_USER:
        {
            CleanupItems(UserVars[varindex].item);
            UserVars[varindex].item = CopyItem(result);
        }
        break;
    case ITV_ARRITEM:
        {
            ExpressionItem *&ei = ((ArrayDesc*)(var->param1))->array[(int)var->param2];
            CleanupItems(ei);
            ei = CopyItem(result);
        }
        break;
    case ITV_STACK:
        {
            ExpressionItem *newitem = CopyItem(result);
            newitem->next = stack;
            stack = newitem;
        }
        break;
    case ITV_NSTACK:
        {
            char *buf = AllocString();
            ItemToString(buf, result);
            pushstring(buf);
            dbgGlobalFree(buf);
        }
        break;
    }
}

void RunAndGetConst(int from, ExpressionItem* &result, int type)
{
    RunTree(*((ExpressionItem**)&(from)), result, type | RTO_NEEDCONST);
    ItemToType(result, type);
}

void RunTree(ExpressionItem *from, ExpressionItem* &result, int options)
{
    ExpressionItem *item = from;
    result = NULL;
    while (item != NULL)
    {
        CleanupItems(result);
        int type = item->type & ITEMTYPE,
            subtype = item->type & ITEMSUBTYPE,
            ioptions = item->type & ITEMOPTIONS;
        switch (type)
        {
        case IT_EXPRESSION:
            RunTree(*((ExpressionItem**)&(item->param1)), result, options);
            break;
        case IT_CONST:
            result = CopyItem(item);
            break;
        case IT_VARIABLE:
            if (options & RTO_NEEDCONST)
            {
                // we need const result - :) is it nsis or common variable
                switch (subtype)
                {
                case ITV_NSIS:
                    {
                        // nsis
                        result = AllocItem();
                        char *variable = getuservariable(ioptions);
                        StringToItem(variable, result, options);
                    }
                    break;
                case ITV_USER:
                    {
                        // usual variable
                        if (UserVars[ioptions].item)
                            result = CopyItem(UserVars[ioptions].item);
                        else
                            result = AllocItem();
                    }
                    break;
                case ITV_ARRITEM:
                    {
                        // array item
                        ExpressionItem *ei = ((ArrayDesc*)(item->param1))->array[(int)item->param2];
                        if (ei)
                            result = CopyItem(ei);
                        else
                            result = AllocItem();
                    }
                    break;
                case ITV_STACK:
                    {
                        // pop from plugin stack
                        result = stack;
                        if (result == NULL) result = AllocItem();
                        stack = result->next;
                        result->next = NULL;
                    }
                    break;
                case ITV_NSTACK:
                    {
                        // NSIS stack
                        char buffer[1024], *buf = buffer;
                        result = AllocItem();
                        popstring(buffer);
                        StringToItem(buf, result, options);
                    }
                    break;
                }
            }
            else
                // if we don't need const - we will just pass variable record
                result = CopyItem(item);
            break;
        case IT_OPERATOR:
            {
                ExpressionItem *var = NULL, *item1 = NULL, *item2 = NULL;
                // prepare arguments in case of SET operator
                if (ioptions & PO_SET)
                {
                    if ((item->param1) && (ioptions & PO_PRENONCONST))
                    {
                        RunTree(*((ExpressionItem**)&(item->param1)), var, 0);
                        if (ioptions & PO_USESPRE)
                            RunTree(var, item1, RTO_NEEDCONST | STI_INT | STI_FLOAT | STI_STRING);
                    } else
                    if ((item->param2) && (ioptions & PO_POSTNONCONST))
                    {
                        RunTree(*((ExpressionItem**)&(item->param2)), var, 0);
                        if (ioptions & PO_USESPOST)
                            RunTree(var, item2, RTO_NEEDCONST | STI_INT | STI_FLOAT | STI_STRING);
                    }
                }

                // prepare arguments in case of any operator
                int needmore = 1;
                if ((!item1) && (item->param1) && (ioptions & PO_USESPRE))
                {
                    RunTree(*((ExpressionItem**)&(item->param1)), item1, RTO_NEEDCONST | STI_INT | STI_FLOAT | STI_STRING);
                    // logical expressions && and || can make decision on first arg basis
                    if ((subtype == ITO_LAND) || (subtype == ITO_LOR) )
                    {
                        ItemToType(item1, ITC_INT);
                        int res = (int) *((__int64*) &(item1->param1));
                        if (((res)&&(subtype==ITO_LOR)) || ((!res)&&(subtype==ITO_LAND)))
                            needmore = 0;
                    }
                }

                // get-reference operator
                if ((!item1) && (subtype == ITO_AND) && (!item2) && (item->param2))
                {
                    RunTree(*((ExpressionItem**)&(item->param2)), result, 0);
                    break;
                }

                if ((needmore) && (!item2) && (item->param2) && (ioptions & PO_USESPOST))
                    RunTree(*((ExpressionItem**)&(item->param2)), item2, RTO_NEEDCONST | STI_INT | STI_FLOAT | STI_STRING);

                // reference operator
                if ((!item1) && (subtype == ITO_MUL) && ((item2->type & ITEMTYPE) == (IT_VARIABLE)))
                {
                    // ok, that's the result we need
                    if (options & RTO_NEEDCONST)
                    {
                        RunTree(item2, result, options);
                        CleanupItems(item2);
                    } else
                        result = item2;
                    break;
                }

                __int64 i1=0, i2=0, i3=0, i4=0;
                if (((!item1)||((item1->type & ITEMTYPE)==IT_CONST)) &&
                    ((!item2)||((item2->type & ITEMTYPE)==IT_CONST)))
                {

                // find the best type match for operation
                int it1 = (item1 && (ioptions & PO_USESPRE))?(item1->type & ITEMSUBTYPE):(ITC_UNKNOWN),
                    it2 = (item2 && (ioptions & PO_USESPOST))?(item2->type & ITEMSUBTYPE):(ITC_UNKNOWN),
                    type = (it1 < it2)?(it1):(it2);

                // convert operands to desired type
                ItemToType(item1, type);
                ItemToType(item2, type);

                switch (type)
                {
                case ITC_INT:
                    {
                        i1 = (item1)?(*((__int64*)&item1->param1)):(0);
                        i2 = (item2)?(*((__int64*)&item2->param1)):(0);

                    switch (subtype)
                    {
                    case ITO_MINUS: i1 -= i2; break;    // unary minus auto handled with NULL
                    case ITO_PLUS:  i1 += i2; break;
                    case ITO_SHL:   i1 <<= i2; break;
                    case ITO_SHR:   i1 >>= i2; break;
                    case ITO_MUL:   i1 *= i2; break;
                    case ITO_MOD:
                    case ITO_DIV:
                                    if (i2 == 0) { i3 = 0; i4 = i1;  }
                                    else { i3 = i1 / i2; i4 = i1 % i2; }
                                    if (subtype == ITO_DIV) i1 = i3; else i1 = i4;
                                    break;
                    case ITO_SET:   i1 = i2; break;
                    case ITO_LE:    i1 = (i1 <= i2); break;
                    case ITO_GE:    i1 = (i1 >= i2); break;
                    case ITO_NE:    i1 = (i1 != i2); break;
                    case ITO_EQ:    i1 = (i1 == i2); break;
                    case ITO_LS:    i1 = (i1 < i2); break;
                    case ITO_GR:    i1 = (i1 > i2); break;
                    case ITO_AND:   i1 = (i1 & i2); break;
                    case ITO_OR:    i1 = (i1 | i2); break;
                    case ITO_XOR:   i1 = (i1 ^ i2); break;
                    case ITO_NOT:   i1 = ~i2; break;
                    case ITO_LNOT:  i1 = !i2; break;
                    case ITO_LAND:  i1 = i1 && i2; break;
                    case ITO_LOR:   i1 = i1 || i2; break;
                    case ITO_INC:
                            if (item1) i2 = i1++;
                            else i1 = ++i2;
                            break;
                    case ITO_DEC:
                            if (item1) i2 = i1--;
                            else i1 = --i2;
                            break;
                    }
                    result = AllocItem();
                    *((__int64*)&result->param1) = i1;
                    }
                    break;
                case ITC_FLOAT:
                    {
                        int ir = -666;
                        double i1 = (item1)?(*((double*)&item1->param1)):(0.0);
                        double i2 = (item2)?(*((double*)&item2->param1)):(0.0);

                    switch (subtype)
                    {
                    case ITO_MINUS: i1 -= i2; break;    // unary minus auto handled with NULL
                    case ITO_PLUS:  i1 += i2; break;
                    case ITO_MUL:   i1 *= i2; break;
                    case ITO_DIV:   i1 /= i2; break;
                    case ITO_SET:   i1 = i2; break;
                    case ITO_LE:    ir = (i1 <= i2); break;
                    case ITO_GE:    ir = (i1 >= i2); break;
                    case ITO_NE:    ir = (i1 != i2); break;
                    case ITO_EQ:    ir = (i1 == i2); break;
                    case ITO_LS:    ir = (i1 < i2); break;
                    case ITO_GR:    ir = (i1 > i2); break;
                    }
                    result = AllocItem();
                    if (ir == -666)
                    {
                        // if ir value left intact - result is double
                        result->type = IT_CONST | ITC_FLOAT;
                        *((double*)&result->param1) = i1;
                    } else
                        *((__int64*)&result->param1) = (__int64) ir;
                    }
                    break;
                case ITC_STRING:
                    {
                        int ir = -666;
                        char *i1 = (item1)?((char*)item1->param1):(NULL);
                        char *i2 = (item2)?((char*)item2->param1):(NULL);
                       	int sc = (i1 && i2)?(lstrcmp(i1, i2)):((i1)?(1):((i2)?(-1):(0)));

                    switch (subtype)
                    {
                    case ITO_PLUS:  lstrcat(i1, i2); break;
                    case ITO_SET:   i1 = i2; break;
                    case ITO_LE:    ir = (sc <= 0); break;
                    case ITO_GE:    ir = (sc >= 0); break;
                    case ITO_NE:    ir = (sc != 0); break;
                    case ITO_EQ:    ir = (sc == 0); break;
                    case ITO_LS:    ir = (sc < 0); break;
                    case ITO_GR:    ir = (sc > 0); break;
                    }
                    if (ir == -666)
                    {
                        result = CopyItem((item1)?(item1):(item2));
                    } else
                    {
                        result = AllocItem();
                        *((__int64*)&result->param1) = (__int64) ir;
                    }
                    }
                    break;
                case ITC_ARRAY:
                    result = CopyItem(item2);
                    break;
                }

                } // check for both items constant
                // the other case - usually UniaryPre operators working with non constants
                else result = CopyItem(item2);

                if (ioptions & PO_SET)
                {
                    // Save our result in output variable
                    SaveResult(var, result);
                }

                // Actual value to be returned as result is at i2 for ++ and -- ops
                if ((subtype == ITO_DEC) || (subtype == ITO_INC))
                    *((__int64*)&result->param1) = i2;

                CleanupItems(item1); CleanupItems(item2); CleanupItems(var);
            }
            break;
        case IT_LOGIC:
            {
                int ifmode = (subtype == ITL_IF);
                ExpressionItem *ifbr = *((ExpressionItem**)&(item->param1)),
                    *dobr = *((ExpressionItem**)&(item->param2)),
                    *thbr = NULL, *elbr = NULL;
                // check do branche for existance
                if (dobr && ifmode)
                {
                    // then...
                    thbr = *((ExpressionItem**)&(dobr->param1));
                    // ... and else branches
                    if (dobr->next) elbr = *((ExpressionItem**)&(dobr->next->param1));
                }
                while (true)
                {
                    RunAndGetConst((int) ifbr, result, ITC_INT);
                    if (ifmode)
                    {
                        // we need then or else branch?
                        if ((*((__int64*)&result->param1))) dobr = thbr;
                        else dobr = elbr;
                    } else
                    {
                        // while mode
                        if (!(*((__int64*)&result->param1))) break;
                    }
                    // ok, run the approtiate branch of if statement (if available)
                    if (dobr)
                    {
                        CleanupItems(result);
                        RunTree(dobr, result, options);
                    }
                    if (ifmode) break;
                    CleanupItems(result);
                }
            }
            break;
        case IT_FUNCTION:
            if (subtype == ITF_USER)
            {
                int i;
                UserFunc *f = &(UserFuncs[ioptions]);
                int flags = f->varflags;
                ExpressionItem *ip = *((ExpressionItem**)&(item->param1));
                ExpressionItem *si = AllocItem(), *var = AllocItem();
                ExpressionItem *vals[32];
                si->type = IT_VARIABLE | ITV_STACK;
                for (i = 0; i < f->varsnum; i++)
                {
                    // push every variable
                    ExpressionItem *val;
                    var->type = (IT_VARIABLE | ITV_USER) + f->vars[i];
                    RunTree(var, val, RTO_NEEDCONST | ITC_STRING | ITC_INT | ITC_FLOAT | ITC_ARRAY);
                    SaveResult(si, val);
                    CleanupItems(val);
                    // calculate argument value and for future
                    if (ip)
                    {
                        if (flags&1)
                        {
                            // var ptr required
                            RunTree(*((ExpressionItem**)&(ip->param1)), vals[i], 0);
                        } else
                        {
                            RunTree(*((ExpressionItem**)&(ip->param1)), vals[i], RTO_NEEDCONST | ITC_STRING | ITC_INT | ITC_FLOAT | ITC_ARRAY);
                        }
                        ip = ip->next;
                    } else vals[i] = AllocItem();

                    flags >>= 1;
                }

                // now when all values got we could save them to variables
                for (i = 0; i < f->varsnum; i++)
                {
                        var->type = (IT_VARIABLE | ITV_USER) + f->vars[i];
                        SaveResult(var, vals[i]);
                        CleanupItems(vals[i]);
                }


                // ok, call the func
                RunTree(f->root, result, RTO_NEEDCONST | ITC_STRING | ITC_INT | ITC_FLOAT | ITC_ARRAY);

                // pop original params
                for (i = f->varsnum-1; i >= 0; i--)
                {
                    // pop every variable
                    ExpressionItem *val;
                    var->type = (IT_VARIABLE | ITV_USER) + f->vars[i];
                    RunTree(si, val, RTO_NEEDCONST | ITC_STRING | ITC_INT | ITC_FLOAT | ITC_ARRAY);
                    SaveResult(var, val);
                    CleanupItems(val);
                }

                // free used items
                CleanupItems(si); CleanupItems(var);
            } else if (subtype == ITF_TYPE)
            {
                int newtype = (int) MathFunctions[ioptions].fptr;
                if (newtype < ITC_UNKNOWN)
                {
                    // get as possibly close to ready expression
                    RunAndGetConst((int)item->param1, result, newtype);
                    if (ioptions == ITFT_CARRAY_ID)
                        CopyArray(result);
                } else if (newtype == FTT_FLOATF)
                {
                    // float format function
                    ExpressionItem *arg1, *arg2;
                    RunAndGetConst((int)item->param1, arg1, ITC_FLOAT);
                    double value = *((double*)&(arg1->param1));
                    RunAndGetConst((int)item->param2, arg2, ITC_INT);
                    int format = (int) *((__int64*)&(arg2->param1));

                    result = AllocItem();
                    result->type = IT_CONST | ITC_STRING;
                    result->param1 = (EIPARAM)  AllocString();
                    FloatFormat((char*) result->param1, value, format);
                    CleanupItems(arg1); CleanupItems(arg2);
                } else if (newtype == FTT_LEN)
                {
                    // length function
                    RunTree(*((ExpressionItem **) &(item->param1)), result, RTO_NEEDCONST | ITC_STRING | ITC_ARRAY);
                    if ((result->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_CONST|ITC_ARRAY))
                    {
                        int len = ((ArrayDesc*)(result->param1))->count;
                        CleanupItems(result);
                        result = AllocItem();
                        *((__int64*)&(result->param1)) = (__int64) len;
                        break;
                    } else
                        if ((result->type & (ITEMTYPE|ITEMSUBTYPE)) != (IT_CONST|ITC_STRING))
                            ItemToType(result, ITC_STRING);

                    if ((result->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_CONST|ITC_STRING))
                    {
                        // ok, that's string
                        int len = lstrlen((char*)result->param1);
                        dbgGlobalFree((HGLOBAL) result->param1);
                        *((__int64*)&(result->param1)) = (__int64) len;
                        result->type = IT_CONST | ITC_INT;
                    } else CleanupItems(result);
                } else
                {
                    // only one left - c() - char/int/char conversion
                    RunTree(*((ExpressionItem **) &(item->param1)), result, RTO_NEEDCONST | ITC_STRING | ITC_INT);
                    if ((result->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_CONST|ITC_STRING))
                    {
                        // ok, that's string - convert first char to int
                        int chr = (*((char*)result->param1)) & 0xFF;
                        dbgGlobalFree((HGLOBAL) result->param1);
                        *((__int64*)&(result->param1)) = (__int64) chr;
                        result->type = IT_CONST | ITC_INT;
                        break;
                    }
                    if ((result->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_CONST|ITC_INT))
                    {
                        // ok, that's int - convert to new string (char+0)
                        int chr = (int) (*((__int64*)&(result->param1))) & 0xFF;
                        result->param1 = (EIPARAM)  AllocString();
                        *((char*)result->param1) = (char) chr;
                        *((char*)(result->param1+1)) = (char) 0;
                        result->type = IT_CONST | ITC_STRING;
                        break;
                    } else CleanupItems(result);
                }
            } else
            {
                // oops :-o function call :)
                RunAndGetConst((int)item->param1, result, ITC_FLOAT);
                double &value = *((double*)&(result->param1));
                if (subtype == ITF_MATH1)
                {
                    // Built-In math function with 1 arg
                    value = MathFunctions[ioptions].fptr(value);
                } else
                if (subtype == ITF_MATH2)
                {
                    // Built-In math function with 2 args
                    if (ioptions >= MATHFUNCNUM-2)
                    {
                        // specific function - we need second arg as out
                        ExpressionItem *arg2, *res2 = AllocItem();
                        RunTree(*((ExpressionItem**)&(item->param2)), arg2, 0);
                        if (ioptions == MATHFUNCNUM-1)
                        {
                            // fmodf function - second arg is ptr to double
                            res2->type = IT_CONST | ITC_FLOAT;
                            double &v = *((double*)&(res2->param1));
                            value = ((Math2dFuncPtr)(MathFunctions[ioptions].fptr))(value, &v);
                        } else
                        {
                            // frexp function - second arg is ptr to int
                            int v = 0;
                            value = ((Math2iFuncPtr)(MathFunctions[ioptions].fptr))(value, &v);
                            *((__int64 *)&(res2->param1)) = (__int64) v;
                        }
                        SaveResult(arg2, res2);
                        CleanupItems(arg2); CleanupItems(res2);
                    } else
                    {
                        // normal 2-arg math function
                        ExpressionItem *arg2;
                        RunAndGetConst((int)item->param2, arg2, ITC_FLOAT);
                        double value2 = *((double*)&(arg2->param1));
                        value = ((Math2FuncPtr)(MathFunctions[ioptions].fptr))(value, value2);
                        CleanupItems(arg2);
                    }
                }
            }
            break;
        case IT_ARRAY:
            {
                // currently only array access is used
                ExpressionItem *index, *aritem;
                RunTree(*((ExpressionItem **) &(item->param1)), aritem, RTO_NEEDCONST | ITC_STRING | ITC_ARRAY);

                if ((aritem->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_CONST | ITC_STRING))
                {
                    // argument is string
                    char *str = (char*)(aritem->param1);
                    int len = lstrlen(str);
                    // have we two indexes or one?
                    if ((*((ExpressionItem **) &(item->param2)))->type != IT_EXPRESSION)
                    {
                        // one index - user need a char
                        RunAndGetConst((int)item->param2, index, ITC_INT);

                        int pos = (int) *((__int64*)&(index->param1));
                        if (pos < 0) pos += len; // -index - means from end
                        if ((pos > len) || (pos < 0))
                            *str = 0; // index is accross string boundaries
                        else
                        {
                            // new string - just a single char
                            *str = *(str+pos);
                            *(str+1) = 0;
                        }
                    } else
                    {
                        // two indexes
                        ExpressionItem *index2;
                        // if first index is skipped -> 0 (from first char)
                        if ((*((ExpressionItem **) &(item->param2)))->param1 == 0)
                            index = AllocItem();
                        else
                            RunAndGetConst((int)(*((ExpressionItem **) &(item->param2)))->param1, index, ITC_INT);
                        if ((*((ExpressionItem **) &(item->param2)))->next->param1 == 0)
                        {
                            // if second index is skipped -> -1 (till last char)
                            index2 = AllocItem();
                            *((__int64*)&(index2->param1)) = -1;
                        }
                        else
                            RunAndGetConst((int)(*((ExpressionItem **) &(item->param2)))->next->param1, index2, ITC_INT);

                        // ok, we've got two indexes
                        int pos1 = (int) *((__int64*)&(index->param1));
                        int pos2 = (int) *((__int64*)&(index2->param1));
                        if (pos1 < 0) pos1 += len; // -index - means from end
                        if (pos2 < 0) pos2 += len; // -index - means from end
                        // limit start/stop positions
                        if (pos1 < 0) pos1 = 0;
                        if (pos2 < 0) pos2 = 0;
                        if (pos1 > len) pos1 = len;
                        if (pos2 >= len) pos2 = len-1;

                        // copy string part
                        char* lpos = str + (pos2-pos1);
                        while (str <= lpos)
                        {
                            *str = *(str + pos1);
                            str++;
                        }
                        // null-termiante
                        *str = 0;

                        CleanupItems(index2);
                    }

                } else
                {
                    // argument is array
                    RunAndGetConst((int)item->param2, index, ITC_INT);

                    // convert array pointer to array item pointer
                    aritem->type = IT_VARIABLE | ITV_ARRITEM;
                    aritem->param2 = (EIPARAM) *((__int64*)&(index->param1));

                    ArrayDesc *ad = (ArrayDesc*)aritem->param1;
                    if (((int)aritem->param2) >= ad->count)
                    {
                        ad->count = ((int)aritem->param2)+1;
                        while (ad->count > ad->size)
                        {
                            // resize array
                            ExpressionItem **oldei = ad->array;
                            ad->array = (ExpressionItem**) dbgGlobalAlloc(GPTR, 2*ad->size*sizeof(ExpressionItem*));
                            for (int i = 0; i < ad->size; i++)
                                ad->array[i] = oldei[i];
                            ad->size*=2;
                            dbgGlobalFree(oldei);
                        }
                    }
                }

                CleanupItems(index);

                // we need constant result?
                if (options & RTO_NEEDCONST)
                {
                    RunTree(aritem, result, options);
                    CleanupItems(aritem);
                } else result = aritem;
            }
            break;
        }
        item = item->next;
    }
}

extern "C"
void __declspec(dllexport) Script(HWND hwndParent, int string_size,
                                      char *variables, stack_t **stacktop)
{
  Math_INIT();
  char *buffer = AllocString(), *buf = buffer;
  ExpressionItem *root = NULL; // root of current tree

  // pop script string
  popstring(buffer);

  // parse it
  ParseString(buf, root);

#ifdef _DEBUG
  // dump
  PrintTree(root, buffer);
#endif

  ExpressionItem *result;
  RunTree(root, result, 0);
  CleanupItems(result);

  CleanupItems(root);
  dbgGlobalFree((HGLOBAL) buffer);
}

double _infinity;

void CleanAll(int init)
{
  if (init)
  {
    unsigned char _infinity_base[8] = {0, 0, 0, 0, 0, 0, 0xf0, 0x7f};
    _fltused = 0;
    _infinity = *((double*)(_infinity_base));
    _fpreset();

    stack = NULL;
    UserVarsCount = 0;
    UserFuncsCount = 0;
  } else
  {
    int i;
    // cleanup stack
    CleanupItems(stack); stack = NULL;
    // cleanup user vars
    for (i = 0; i < UserVarsCount; i++)
        CleanupItems(UserVars[i].item);
    // cleanup user funcs
    for (i = 0; i < UserFuncsCount; i++)
        CleanupItems(UserFuncs[i].root);
    UserVarsCount = 0;
    UserFuncsCount = 0;

    dbgGlobalCheck();
  }
}

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
    CleanAll(ul_reason_for_call == DLL_PROCESS_ATTACH);
    return TRUE;
}

// crt0.obj is linked in and requires this
int main()
{
  return 0;
}
