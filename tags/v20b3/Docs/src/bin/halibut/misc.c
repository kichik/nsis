/*
 * misc.c: miscellaneous useful items
 */

#include "halibut.h"

struct stackTag {
    void **data;
    int sp;
    int size;
};

stack stk_new(void)
{
    stack s;

    s = mknew(struct stackTag);
    s->sp = 0;
    s->size = 0;
    s->data = NULL;

    return s;
}

void stk_free(stack s)
{
    sfree(s->data);
    sfree(s);
}

void stk_push(stack s, void *item)
{
    if (s->size <= s->sp) {
	s->size = s->sp + 32;
	s->data = resize(s->data, s->size);
    }
    s->data[s->sp++] = item;
}

void *stk_pop(stack s)
{
    if (s->sp > 0)
	return s->data[--s->sp];
    else
	return NULL;
}

/*
 * Small routines to amalgamate a string from an input source.
 */
const rdstring empty_rdstring = { 0, 0, NULL };
const rdstringc empty_rdstringc = { 0, 0, NULL };

void rdadd(rdstring * rs, wchar_t c)
{
    if (rs->pos >= rs->size - 1) {
	rs->size = rs->pos + 128;
	rs->text = resize(rs->text, rs->size);
    }
    rs->text[rs->pos++] = c;
    rs->text[rs->pos] = 0;
}

void rdadds(rdstring * rs, wchar_t * p)
{
    int len = ustrlen(p);
    if (rs->pos >= rs->size - len) {
	rs->size = rs->pos + len + 128;
	rs->text = resize(rs->text, rs->size);
    }
    ustrcpy(rs->text + rs->pos, p);
    rs->pos += len;
}

wchar_t *rdtrim(rdstring * rs)
{
    rs->text = resize(rs->text, rs->pos + 1);
    return rs->text;
}

void rdaddc(rdstringc * rs, char c)
{
    if (rs->pos >= rs->size - 1) {
	rs->size = rs->pos + 128;
	rs->text = resize(rs->text, rs->size);
    }
    rs->text[rs->pos++] = c;
    rs->text[rs->pos] = 0;
}

void rdaddsc(rdstringc * rs, char *p)
{
    int len = strlen(p);
    if (rs->pos >= rs->size - len) {
	rs->size = rs->pos + len + 128;
	rs->text = resize(rs->text, rs->size);
    }
    strcpy(rs->text + rs->pos, p);
    rs->pos += len;
}

char *rdtrimc(rdstringc * rs)
{
    rs->text = resize(rs->text, rs->pos + 1);
    return rs->text;
}

int compare_wordlists(word * a, word * b)
{
    int t;
    while (a && b) {
	if (a->type != b->type)
	    return (a->type < b->type ? -1 : +1);	/* FIXME? */
	t = a->type;
	if ((t != word_Normal && t != word_Code &&
	     t != word_WeakCode && t != word_Emph) || a->alt || b->alt) {
	    int c;
	    if (a->text && b->text) {
		c = ustricmp(a->text, b->text);
		if (c)
		    return c;
	    }
	    c = compare_wordlists(a->alt, b->alt);
	    if (c)
		return c;
	    a = a->next;
	    b = b->next;
	} else {
	    wchar_t *ap = a->text, *bp = b->text;
	    while (*ap && *bp) {
		wchar_t ac = utolower(*ap), bc = utolower(*bp);
		if (ac != bc)
		    return (ac < bc ? -1 : +1);
		if (!*++ap && a->next && a->next->type == t
		    && !a->next->alt)
		    a = a->next, ap = a->text;
		if (!*++bp && b->next && b->next->type == t
		    && !b->next->alt)
		    b = b->next, bp = b->text;
	    }
	    if (*ap || *bp)
		return (*ap ? +1 : -1);
	    a = a->next;
	    b = b->next;
	}
    }

    if (a || b)
	return (a ? +1 : -1);
    else
	return 0;
}

void mark_attr_ends(paragraph * sourceform)
{
    paragraph *p;
    word *w, *wp;
    for (p = sourceform; p; p = p->next) {
	wp = NULL;
	for (w = p->words; w; w = w->next) {
	    if (isattr(w->type)) {
		int before = (wp && isattr(wp->type) &&
			      sameattr(wp->type, w->type));
		int after = (w->next && isattr(w->next->type) &&
			     sameattr(w->next->type, w->type));
		w->aux |= (before ?
			   (after ? attr_Always : attr_Last) :
			   (after ? attr_First : attr_Only));
	    }
	    wp = w;
	}
    }
}

wrappedline *wrap_para(word * text, int width, int subsequentwidth,
		       int (*widthfn) (word *))
{
    wrappedline *head = NULL, **ptr = &head;
    int nwords, wordsize;
    struct wrapword {
	word *begin, *end;
	int width;
	int spacewidth;
	int cost;
	int nwords;
    } *wrapwords;
    int i, j, n;

    /*
     * Break the line up into wrappable components.
     */
    nwords = wordsize = 0;
    wrapwords = NULL;
    while (text) {
	if (nwords >= wordsize) {
	    wordsize = nwords + 64;
	    wrapwords = srealloc(wrapwords, wordsize * sizeof(*wrapwords));
	}
	wrapwords[nwords].width = 0;
	wrapwords[nwords].begin = text;
	while (text) {
	    wrapwords[nwords].width += widthfn(text);
	    wrapwords[nwords].end = text->next;
	    if (text->next && (text->next->type == word_WhiteSpace ||
			       text->next->type == word_EmphSpace ||
			       text->breaks))
		break;
	    text = text->next;
	}
	if (text && text->next && (text->next->type == word_WhiteSpace ||
				   text->next->type == word_EmphSpace)) {
	    wrapwords[nwords].spacewidth = widthfn(text->next);
	    text = text->next;
	} else {
	    wrapwords[nwords].spacewidth = 0;
	}
	nwords++;
	if (text)
	    text = text->next;
    }

    /*
     * Perform the dynamic wrapping algorithm: work backwards from
     * nwords-1, determining the optimal wrapping for each terminal
     * subsequence of the paragraph.
     */
    for (i = nwords; i--;) {
	int best = -1;
	int bestcost = 0;
	int cost;
	int linelen = 0, spacewidth = 0;
	int seenspace;
	int thiswidth = (i == 0 ? width : subsequentwidth);

	j = 0;
	seenspace = 0;
	while (i + j < nwords) {
	    /*
	     * See what happens if we put j+1 words on this line.
	     */
	    if (spacewidth)
		seenspace = 1;
	    linelen += spacewidth + wrapwords[i + j].width;
	    spacewidth = wrapwords[i + j].spacewidth;
	    j++;
	    if (linelen > thiswidth) {
		/*
		 * If we're over the width limit, abandon ship,
		 * _unless_ there is no best-effort yet (which will
		 * only happen if the first word is too long all by
		 * itself).
		 */
		if (best > 0)
		    break;
	    }
	    if (i + j == nwords) {
		/*
		 * Special case: if we're at the very end of the
		 * paragraph, we don't score penalty points for the
		 * white space left on the line.
		 */
		cost = 0;
	    } else {
		cost = (thiswidth - linelen) * (thiswidth - linelen);
		cost += wrapwords[i + j].cost;
	    }
	    /*
	     * We compare bestcost >= cost, not bestcost > cost,
	     * because in cases where the costs are identical we
	     * want to try to look like the greedy algorithm,
	     * because readers are likely to have spent a lot of
	     * time looking at greedy-wrapped paragraphs and
	     * there's no point violating the Principle of Least
	     * Surprise if it doesn't actually gain anything.
	     */
	    if (best < 0 || bestcost >= cost) {
		bestcost = cost;
		best = j;
	    }
	}
	/*
	 * Now we know the optimal answer for this terminal
	 * subsequence, so put it in wrapwords.
	 */
	wrapwords[i].cost = bestcost;
	wrapwords[i].nwords = best;
    }

    /*
     * We've wrapped the paragraph. Now build the output
     * `wrappedline' list.
     */
    i = 0;
    while (i < nwords) {
	wrappedline *w = mknew(wrappedline);
	*ptr = w;
	ptr = &w->next;
	w->next = NULL;

	n = wrapwords[i].nwords;
	w->begin = wrapwords[i].begin;
	w->end = wrapwords[i + n - 1].end;

	/*
	 * Count along the words to find nspaces and shortfall.
	 */
	w->nspaces = 0;
	w->shortfall = width;
	for (j = 0; j < n; j++) {
	    w->shortfall -= wrapwords[i + j].width;
	    if (j < n - 1 && wrapwords[i + j].spacewidth) {
		w->nspaces++;
		w->shortfall -= wrapwords[i + j].spacewidth;
	    }
	}
	i += n;
    }

    sfree(wrapwords);

    return head;
}

void wrap_free(wrappedline * w)
{
    while (w) {
	wrappedline *t = w->next;
	sfree(w);
	w = t;
    }
}
