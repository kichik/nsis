#pragma once

#define DEFAULT_ARRAY_SIZE  1024

#define ITEMTYPE        0xFF0000
// items classes
#define IT_CONST        0x000000
#define IT_EXPRESSION   0x010000
#define IT_OPERATOR     0x020000
#define IT_VARIABLE     0x030000
#define IT_LOGIC        0x040000    // flow control items
#define IT_FUNCTION     0x050000
#define IT_ARRAY        0x060000    // array operation

#define ITEMSUBTYPE 0x00FF00
// const items
#define ITC_STRING  0x000100
#define ITC_FLOAT   0x000200
#define ITC_INT     0x000400
#define ITC_ARRAY   0x000800
#define ITC_VARPTR  0x001000
#define ITC_UNKNOWN 0x002000

// type function
#define FTT_FLOATF  (ITC_UNKNOWN << 0)
#define FTT_LEN     (ITC_UNKNOWN << 1)
#define FTT_CHAR    (ITC_UNKNOWN << 2)

// additional option - for "ca" function
#define ITFA_COPY   0x000001

// ops items
#define ITO_MINUS   0x000100
#define ITO_PLUS    0x000200
#define ITO_SHL     0x000300
#define ITO_SHR     0x000400
#define ITO_MUL     0x000500
#define ITO_DIV     0x000600
#define ITO_SET     0x000700
#define ITO_LAND    0x000800
#define ITO_LOR     0x000900
#define ITO_INC     0x000A00
#define ITO_DEC     0x000B00
#define ITO_LE      0x000C00
#define ITO_GE      0x000D00
#define ITO_NE      0x000E00
#define ITO_EQ      0x000F00
#define ITO_LS      0x001000
#define ITO_GR      0x001100
#define ITO_AND     0x001200
#define ITO_MOD     0x001300
#define ITO_OR      0x001400
#define ITO_XOR     0x001500
#define ITO_NOT     0x001600
#define ITO_LNOT    0x001700

// variables sub-types
#define ITV_NSIS    0x000100
#define ITV_USER    0x000200
#define ITV_ARRITEM 0x000400
#define ITV_STACK   0x000800    // plugin specific stack
#define ITV_NSTACK  0x001000    // nsis stack

// logic sub-types
#define ITL_IF      0x000100
#define ITL_WHILE   0x000200

// function sub-types
#define ITF_MATH1   0x000100
#define ITF_MATH2   0x000200
#define ITF_TYPE    0x000300
#define ITF_USER    0x000400

// array items sub-types
#define ITA_DEFINE  0x000100
#define ITA_ACCESS  0x000200

#define ITEMOPTIONS 0x0000FF

// 16 bytes structure
typedef struct __ExpressionItem ExpressionItem;
typedef struct __ExpressionItem
{
    int type;
    int param1;
    int param2;
    ExpressionItem *next;
} ExpressionItem;

typedef struct __ParseInfo
{
int SetupNewRoot;
ExpressionItem *item;
ExpressionItem *OpsStack;
ExpressionItem* &place;
ExpressionItem **root;
char valbuf[108];
} ParseInfo;

typedef struct __OpStruct
{
    char name[4];
    unsigned short int type;
} OpStruct;

#define MAX_USER_VARS   256
typedef struct __UserVar
{
    char name[28];
    ExpressionItem *item;
} UserVar;

#define MAX_USER_FUNCS  256
typedef struct __UserFunc
{
    char name[20];
    unsigned char vars[31];
    unsigned char varsnum;
    unsigned int varflags;
    ExpressionItem *root;
} UserFunc;

typedef struct __ArrayDesc
{
    ExpressionItem **array;
    int size;   // size of allocated items pool
    int count;  // max number of item accessed
    int references; // array will be killed at CleanupItems only when references == 0
} ArrayDesc;

typedef double (*Math1FuncPtr)(double arg);
typedef double (*Math2FuncPtr)(double arg, double arg2);
typedef double (*Math2iFuncPtr)(double arg, int *arg2);
typedef double (*Math2dFuncPtr)(double arg, double *arg2);

typedef struct __MathFunction
{
    char name[3];
    unsigned char type;
    Math1FuncPtr fptr;
} MathFunction;

#define STI_STRING   0x0100
#define STI_FLOAT    0x0200
#define STI_INT      0x0400

#define FF_DEFAULT   0x00     // uses default mode: if available noexp, else exp
#define FF_NOEXP     0x10     // uses noexp mode
#define FF_EXP       0x20     // uses exp mode (small e)
#define FF_LEXP      0x40     // uses exp mode (large E)

// parsestring options
#define PSO_STOPATDELIMETER 0x1

// RunTree options
#define RTO_NEEDCONST   0x0001
#define RTO_PREFFEREDTYPE   0xFF00
void RunTree(ExpressionItem *from, ExpressionItem* &result, int type);

void StringToItem(char *&sbuf, ExpressionItem *item, int options);
void ItemToString(char *sbuf, ExpressionItem *item);
void FloatFormat(char *sbuf, double value, int options);
void itoa64(__int64 i, char *buffer);
int lstrcmpn(char *s1, const char *s2, int chars);