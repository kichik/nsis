/*
 * malloc.c: safe wrappers around malloc, realloc, free, strdup
 */

#include <stdlib.h>
#include <stdarg.h>
#include "halibut.h"

#ifdef LOGALLOC
#define LOGPARAMS char *file, int line,
static FILE *logallocfp = NULL;
static int logline = 2;		/* off by 1: `null pointer is' */
static void loginc(void)
{
}
static void logallocinit(void)
{
    if (!logallocfp) {
	logallocfp = fopen("malloc.log", "w");
	if (!logallocfp) {
	    fprintf(stderr, "panic: unable to open malloc.log\n");
	    exit(10);
	}
	setvbuf(logallocfp, NULL, _IOLBF, BUFSIZ);
	fprintf(logallocfp, "null pointer is %p\n", NULL);
    }
}
static void logprintf(char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(logallocfp, fmt, ap);
    va_end(ap);
}

#define LOGPRINT(x) ( logallocinit(), logprintf x )
#define LOGINC do { loginc(); logline++; } while (0)
#else
#define LOGPARAMS
#define LOGPRINT(x)
#define LOGINC ((void)0)
#endif

/*
 * smalloc should guarantee to return a useful pointer - Halibut
 * can do nothing except die when it's out of memory anyway.
 */
void *(smalloc) (LOGPARAMS int size) {
    void *p;
    LOGINC;
    LOGPRINT(("%s %d malloc(%ld)", file, line, (long) size));
    p = malloc(size);
    if (!p)
	fatal(err_nomemory);
    LOGPRINT((" returns %p\n", p));
    return p;
}

/*
 * sfree should guaranteeably deal gracefully with freeing NULL
 */
void (sfree) (LOGPARAMS void *p) {
    if (p) {
	LOGINC;
	LOGPRINT(("%s %d free(%p)\n", file, line, p));
	free(p);
    }
}

/*
 * srealloc should guaranteeably be able to realloc NULL
 */
void *(srealloc) (LOGPARAMS void *p, int size) {
    void *q;
    if (p) {
	LOGINC;
	LOGPRINT(("%s %d realloc(%p,%ld)", file, line, p, (long) size));
	q = realloc(p, size);
	LOGPRINT((" returns %p\n", q));
    } else {
	LOGINC;
	LOGPRINT(("%s %d malloc(%ld)", file, line, (long) size));
	q = malloc(size);
	LOGPRINT((" returns %p\n", q));
    }
    if (!q)
	fatal(err_nomemory);
    return q;
}

/*
 * dupstr is like strdup, but with the never-return-NULL property
 * of smalloc (and also reliably defined in all environments :-)
 */
char *dupstr(char *s)
{
    char *r = smalloc(1 + strlen(s));
    strcpy(r, s);
    return r;
}

/*
 * Duplicate a linked list of words
 */
word *dup_word_list(word * w)
{
    word *head, **eptr = &head;

    while (w) {
	word *newwd = mknew(word);
	*newwd = *w;		/* structure copy */
	newwd->text = ustrdup(w->text);
	if (w->alt)
	    newwd->alt = dup_word_list(w->alt);
	*eptr = newwd;
	newwd->next = NULL;
	eptr = &newwd->next;

	w = w->next;
    }

    return head;
}

/*
 * Free a linked list of words
 */
void free_word_list(word * w)
{
    word *t;
    while (w) {
	t = w;
	w = w->next;
	sfree(t->text);
	if (t->alt)
	    free_word_list(t->alt);
	sfree(t);
    }
}

/*
 * Free a linked list of paragraphs
 */
void free_para_list(paragraph * p)
{
    paragraph *t;
    while (p) {
	t = p;
	p = p->next;
	sfree(t->keyword);
	free_word_list(t->words);
	sfree(t);
    }
}
