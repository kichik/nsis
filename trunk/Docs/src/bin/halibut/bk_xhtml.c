/*
 * xhtml backend for Halibut
 * (initial implementation by James Aylett)
 *
 * Still to do:
 *
 *  +++ doesn't handle non-breaking hyphens. Not sure how to yet.
 *  +++ entity names (from a file -- ideally supply normal SGML files)
 *  +++ configuration directive to file split where the current layout
 *      code wouldn't. Needs changes to _ponder_layout() and _do_paras(),
 *      perhaps others.
 *
 * Limitations:
 *
 *  +++ biblio/index references target the nearest section marker, rather
 *   than having a dedicated target themselves. In large bibliographies
 *   this will cause problems. (The solution is to fake up a response
 *   from xhtml_find_section(), probably linking it into the sections
 *   chain just in case we need it again, and to make freeing it up
 *   easier.) docsrc.pl used to work as we do, however, and SGT agrees that
 *   this is acceptable for now.
 *  +++ can't cope with leaf-level == 0. It's all to do with the
 *   top-level file not being normal, probably not even having a valid
 *   section level, and stuff like that. I question whether this is an
 *   issue, frankly; small manuals that fit on one page should probably
 *   not be written in halibut at all.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "halibut.h"

struct xhtmlsection_Struct {
    struct xhtmlsection_Struct *next;	/* next sibling (NULL if split across files) */
    struct xhtmlsection_Struct *child;	/* NULL if split across files */
    struct xhtmlsection_Struct *parent;	/* NULL if split across files */
    struct xhtmlsection_Struct *chain;	/* single structure independent of weird trees */
    paragraph *para;
    struct xhtmlfile_Struct *file;	/* which file is this a part of? */
    char *fragment;		/* fragment id within the file */
    int level;
};

struct xhtmlfile_Struct {
    struct xhtmlfile_Struct *next;
    struct xhtmlfile_Struct *child;
    struct xhtmlfile_Struct *parent;
    char *filename;
    struct xhtmlsection_Struct *sections;	/* sections within this file (only one for non-leaf) */
    int is_leaf;		/* is this file a leaf file, ie does it not have any children? */
};

typedef struct xhtmlsection_Struct xhtmlsection;
typedef struct xhtmlfile_Struct xhtmlfile;
typedef struct xhtmlindex_Struct xhtmlindex;

struct xhtmlindex_Struct {
    int nsection;
    int size;
    xhtmlsection **sections;
};

typedef struct {
    int just_numbers;
    wchar_t *number_suffix;
} xhtmlheadfmt;

typedef struct {
    int contents_depth[6];
    int leaf_contains_contents;
    int leaf_level;
    int leaf_smallest_contents;
    int include_version_id;
    wchar_t *author, *description;
    wchar_t *head_end, *body, *body_start, *body_end, *address_start,
	*address_end, *nav_attrs;
    int suppress_address;
    xhtmlheadfmt fchapter, *fsect;
    int nfsect;
} xhtmlconfig;

/*static void xhtml_level(paragraph *, int);
static void xhtml_level_0(paragraph *);
static void xhtml_docontents(FILE *, paragraph *, int);
static void xhtml_dosections(FILE *, paragraph *, int);
static void xhtml_dobody(FILE *, paragraph *, int);*/

static void xhtml_doheader(FILE *, word *);
static void xhtml_dofooter(FILE *);
static void xhtml_versionid(FILE *, word *, int);

static void xhtml_utostr(wchar_t *, char **);
static int xhtml_para_level(paragraph *);
static int xhtml_reservedchar(int);

static int xhtml_convert(wchar_t *, char **, int);
static void xhtml_rdaddwc(rdstringc *, word *, word *);
static void xhtml_para(FILE *, word *);
static void xhtml_codepara(FILE *, word *);
static void xhtml_heading(FILE *, paragraph *);

/* File-global variables are much easier than passing these things
 * all over the place. Evil, but easier. We can replace this with a single
 * structure at some point.
 */
static xhtmlconfig conf;
static keywordlist *keywords;
static indexdata *idx;
static xhtmlfile *topfile;
static xhtmlsection *topsection;
static paragraph *sourceparas;
static xhtmlfile *lastfile;
static xhtmlfile *xhtml_last_file = NULL;
static int last_level = -1;
static xhtmlsection *currentsection;

static xhtmlconfig xhtml_configure(paragraph * source)
{
    xhtmlconfig ret;

    /*
     * Defaults.
     */
    ret.contents_depth[0] = 2;
    ret.contents_depth[1] = 3;
    ret.contents_depth[2] = 4;
    ret.contents_depth[3] = 5;
    ret.contents_depth[4] = 6;
    ret.contents_depth[5] = 7;
    ret.leaf_level = 2;
    ret.leaf_smallest_contents = 4;
    ret.leaf_contains_contents = FALSE;
    ret.include_version_id = TRUE;
    ret.author = NULL;
    ret.description = NULL;
    ret.head_end = NULL;
    ret.body = NULL;
    ret.body_start = NULL;
    ret.body_end = NULL;
    ret.address_start = NULL;
    ret.address_end = NULL;
    ret.nav_attrs = NULL;
    ret.suppress_address = FALSE;

    ret.fchapter.just_numbers = FALSE;
    ret.fchapter.number_suffix = ustrdup(L": ");
    ret.nfsect = 2;
    ret.fsect = mknewa(xhtmlheadfmt, ret.nfsect);
    ret.fsect[0].just_numbers = FALSE;
    ret.fsect[0].number_suffix = ustrdup(L": ");
    ret.fsect[1].just_numbers = TRUE;
    ret.fsect[1].number_suffix = ustrdup(L" ");

    for (; source; source = source->next) {
	if (source->type == para_Config) {
	    if (!ustricmp(source->keyword, L"xhtml-contents-depth-0")) {
		ret.contents_depth[0] = utoi(uadv(source->keyword));
	    } else
		if (!ustricmp(source->keyword, L"xhtml-contents-depth-1"))
	    {
		ret.contents_depth[1] = utoi(uadv(source->keyword));
	    } else
		if (!ustricmp(source->keyword, L"xhtml-contents-depth-2"))
	    {
		ret.contents_depth[2] = utoi(uadv(source->keyword));
	    } else
		if (!ustricmp(source->keyword, L"xhtml-contents-depth-3"))
	    {
		ret.contents_depth[3] = utoi(uadv(source->keyword));
	    } else
		if (!ustricmp(source->keyword, L"xhtml-contents-depth-4"))
	    {
		ret.contents_depth[4] = utoi(uadv(source->keyword));
	    } else
		if (!ustricmp(source->keyword, L"xhtml-contents-depth-5"))
	    {
		ret.contents_depth[5] = utoi(uadv(source->keyword));
	    } else if (!ustricmp(source->keyword, L"xhtml-leaf-level")) {
		ret.leaf_level = utoi(uadv(source->keyword));
	    } else
		if (!ustricmp
		    (source->keyword, L"xhtml-leaf-smallest-contents")) {
		ret.leaf_smallest_contents = utoi(uadv(source->keyword));
	    } else if (!ustricmp(source->keyword, L"xhtml-versionid")) {
		ret.include_version_id = utob(uadv(source->keyword));
	    } else
		if (!ustricmp
		    (source->keyword, L"xhtml-leaf-contains-contents")) {
		ret.leaf_contains_contents = utob(uadv(source->keyword));
	    } else
		if (!ustricmp(source->keyword, L"xhtml-suppress-address"))
	    {
		ret.suppress_address = utob(uadv(source->keyword));
	    } else if (!ustricmp(source->keyword, L"xhtml-author")) {
		ret.author = uadv(source->keyword);
	    } else if (!ustricmp(source->keyword, L"xhtml-description")) {
		ret.description = uadv(source->keyword);
	    } else if (!ustricmp(source->keyword, L"xhtml-head-end")) {
		ret.head_end = uadv(source->keyword);
	    } else if (!ustricmp(source->keyword, L"xhtml-body-start")) {
		ret.body_start = uadv(source->keyword);
	    } else if (!ustricmp(source->keyword, L"xhtml-body-tag")) {
		ret.body = uadv(source->keyword);
	    } else if (!ustricmp(source->keyword, L"xhtml-body-end")) {
		ret.body_end = uadv(source->keyword);
	    } else if (!ustricmp(source->keyword, L"xhtml-address-start")) {
		ret.address_start = uadv(source->keyword);
	    } else if (!ustricmp(source->keyword, L"xhtml-address-end")) {
		ret.address_end = uadv(source->keyword);
	    } else
		if (!ustricmp
		    (source->keyword, L"xhtml-navigation-attributes")) {
		ret.nav_attrs = uadv(source->keyword);
	    } else
		if (!ustricmp(source->keyword, L"xhtml-chapter-numeric")) {
		ret.fchapter.just_numbers = utob(uadv(source->keyword));
	    } else if (!ustricmp(source->keyword, L"xhtml-chapter-suffix")) {
		ret.fchapter.number_suffix =
		    ustrdup(uadv(source->keyword));
	    } else
		if (!ustricmp(source->keyword, L"xhtml-section-numeric")) {
		wchar_t *p = uadv(source->keyword);
		int n = 0;
		if (uisdigit(*p)) {
		    n = utoi(p);
		    p = uadv(p);
		}
		if (n >= ret.nfsect) {
		    int i;
		    ret.fsect = resize(ret.fsect, n + 1);
		    for (i = ret.nfsect; i <= n; i++)
			ret.fsect[i] = ret.fsect[ret.nfsect - 1];
		    ret.nfsect = n + 1;
		}
		ret.fsect[n].just_numbers = utob(p);
	    } else if (!ustricmp(source->keyword, L"xhtml-section-suffix")) {
		wchar_t *p = uadv(source->keyword);
		int n = 0;
		if (uisdigit(*p)) {
		    n = utoi(p);
		    p = uadv(p);
		}
		if (n >= ret.nfsect) {
		    int i;
		    ret.fsect = resize(ret.fsect, n + 1);
		    for (i = ret.nfsect; i <= n; i++)
			ret.fsect[i] = ret.fsect[ret.nfsect - 1];
		    ret.nfsect = n + 1;
		}
		ret.fsect[n].number_suffix = ustrdup(p);
	    }
	}
    }

    /*  printf(" !!! leaf_level = %i\n", ret.leaf_level);
       printf(" !!! contentdepth-0 = %i\n", ret.contents_depth[0]);
       printf(" !!! contentdepth-1 = %i\n", ret.contents_depth[1]);
       printf(" !!! contentdepth-2 = %i\n", ret.contents_depth[2]);
       printf(" !!! contentdepth-3 = %i\n", ret.contents_depth[3]);
       printf(" !!! contentdepth-4 = %i\n", ret.contents_depth[4]);
       printf(" !!! contentdepth-5 = %i\n", ret.contents_depth[5]);
       printf(" !!! leaf_contains_contents = %i\n", ret.leaf_contains_contents); */
    return ret;
}

static xhtmlsection *xhtml_new_section(xhtmlsection * last)
{
    xhtmlsection *ret = mknew(xhtmlsection);
    ret->next = NULL;
    ret->child = NULL;
    ret->parent = NULL;
    ret->chain = last;
    ret->para = NULL;
    ret->file = NULL;
    ret->fragment = NULL;
    ret->level = -1;		/* marker: end of chain */
    return ret;
}

/* Returns NULL or the section that marks that paragraph */
static xhtmlsection *xhtml_find_section(paragraph * p)
{
    xhtmlsection *ret = topsection;
    if (xhtml_para_level(p) == -1) {	/* first, we back-track to a section paragraph */
	paragraph *p2 = sourceparas;
	paragraph *p3 = NULL;
	while (p2 && p2 != p) {
	    if (xhtml_para_level(p2) != -1) {
		p3 = p2;
	    }
	    p2 = p2->next;
	}
	if (p3 == NULL) {	/* for some reason, we couldn't find a section before this paragraph ... ? */
	    /* Note that this can happen, if you have a cross-reference to before the first chapter starts.
	     * So don't do that, then.
	     */
	    return NULL;
	}
	p = p3;
    }
    while (ret && ret->para != p) {
/*    printf(" xhtml_find_section(): checking %s for para @ %p\n", ret->fragment, p);*/
	ret = ret->chain;
    }
    return ret;
}

static xhtmlfile *xhtml_new_file(xhtmlsection * sect)
{
    xhtmlfile *ret = mknew(xhtmlfile);

    ret->next = NULL;
    ret->child = NULL;
    ret->parent = NULL;
    ret->filename = NULL;
    ret->sections = sect;
    ret->is_leaf = (sect != NULL && sect->level == conf.leaf_level);
    if (sect == NULL) {
	if (conf.leaf_level == 0) {	/* currently unused */
#define FILENAME_MANUAL "Manual.html"
#define FILENAME_CONTENTS "Contents.html"
	    ret->filename = smalloc(strlen(FILENAME_MANUAL) + 1);
	    sprintf(ret->filename, FILENAME_MANUAL);
	} else {
	    ret->filename = smalloc(strlen(FILENAME_CONTENTS) + 1);
	    sprintf(ret->filename, FILENAME_CONTENTS);
	}
    } else {
	paragraph *p = sect->para;
	rdstringc fname_c = { 0, 0, NULL };
	char *c;
	word *w;
	for (w = (p->kwtext) ? (p->kwtext) : (p->words); w; w = w->next) {
	    switch (removeattr(w->type)) {
	    case word_Normal:
		/*case word_Emph:
		   case word_Code:
		   case word_WeakCode: */
		xhtml_utostr(w->text, &c);
		rdaddsc(&fname_c, c);
		sfree(c);
		break;
	    }
	}
	rdaddsc(&fname_c, ".html");
	ret->filename = rdtrimc(&fname_c);
    }
    /*  printf(" ! new file '%s', is_leaf == %s\n", ret->filename, (ret->is_leaf)?("true"):("false")); */
    return ret;
}

/*
 * Walk the tree fixing up files which are actually leaf (ie
 * have no children) but aren't at leaf level, so they have the
 * leaf flag set.
 */
void xhtml_fixup_layout(xhtmlfile * file)
{
    if (file->child == NULL) {
	file->is_leaf = TRUE;
    } else {
	xhtml_fixup_layout(file->child);
    }
    if (file->next)
	xhtml_fixup_layout(file->next);
}

/*
 * Create the tree structure so we know where everything goes.
 * Method:
 *
 * Ignoring file splitting, we have three choices with each new section:
 * 
 * +-----------------+-----------------+
 * |                 |                 |
 * X            +----X----+           (1)
 *              |         |
 *              Y        (2)
 *              |
 *             (3)
 *
 * Y is the last section we added (currentsect).
 * If sect is the section we want to add, then:
 *
 * (1) if sect->level < currentsect->level
 * (2) if sect->level == currentsect->level
 * (3) if sect->level > currentsect->level
 *
 * This requires the constraint that you never skip section numbers
 * (so you can't have a.b.c.d without all of a, a.b and a.b.c existing).
 *
 * Note that you _can_ have 1.1.1.1 followed by 1.2 - you can change
 * more than one level at a time. Lots of asserts, and probably part of
 * the algorithm here, rely on this being true. (It currently isn't
 * enforced by halibut, however.)
 *
 * File splitting makes this harder. For instance, say we added at (3)
 * above and now need to add another section. We are splitting at level
 * 2, ie the level of Y. Z is the last section we added:
 *
 * +-----------------+-----------------+
 * |                 |                 |
 * X            +----X----+           (1)
 *              |         |
 *         +----Y----+   (1)
 *         |         |
 *         Z        (2)
 *         |
 *        (3)
 *
 * The (1) case is now split; we need to search upwards to find where
 * to actually link in. The other two cases remain the same (and will
 * always be like this).
 *
 * File splitting makes this harder, however. The decision of whether
 * to split to a new file is always on the same condition, however (is
 * the level of this section higher than the leaf_level configuration
 * value or not).
 *
 * Treating the cases backwards:
 *
 * (3) same file if sect->level > conf.leaf_level, otherwise new file
 *
 *     if in the same file, currentsect->child points to sect
 *     otherwise the linking is done through the file tree (which works
 *     in more or less the same way, ie currentfile->child points to
 *     the new file)
 *
 * (2) same file if sect->level > conf.leaf_level, otherwise new file
 *
 *     if in the same file, currentsect->next points to sect
 *     otherwise file linking and currentfile->next points to the new
 *     file (we know that Z must have caused a new file to be created)
 *
 * (1) same file if sect->level > conf.leaf_level, otherwise new file
 *
 *     this is actually effectively the same case as (2) here,
 *     except that we first have to travel up the sections to figure
 *     out which section this new one will be a sibling of. In doing
 *     so, we may disappear off the top of a file and have to go up
 *     to its parent in the file tree.
 *
 */
static void xhtml_ponder_layout(paragraph * p)
{
    xhtmlsection *lastsection;
    xhtmlsection *currentsect;
    xhtmlfile *currentfile;

    lastfile = NULL;
    topsection = xhtml_new_section(NULL);
    topfile = xhtml_new_file(NULL);
    lastsection = topsection;
    currentfile = topfile;
    currentsect = topsection;

    if (conf.leaf_level == 0) {
	topfile->is_leaf = 1;
	topfile->sections = topsection;
	topsection->file = topfile;
    }

    for (; p; p = p->next) {
	int level = xhtml_para_level(p);
	if (level > 0) {	/* actually a section */
	    xhtmlsection *sect;
	    word *w;
	    char *c;
	    rdstringc fname_c = { 0, 0, NULL };

	    sect = xhtml_new_section(lastsection);
	    lastsection = sect;
	    sect->para = p;
	    for (w = (p->kwtext2) ? (p->kwtext2) : (p->words); w; w = w->next) {	/* kwtext2 because we want numbers only! */
		switch (removeattr(w->type)) {
		case word_Normal:
		    /*case word_Emph:
		       case word_Code:
		       case word_WeakCode: */
		    xhtml_utostr(w->text, &c);
		    rdaddsc(&fname_c, c);
		    sfree(c);
		    break;
		}
	    }
/*      rdaddsc(&fname_c, ".html");*/
	    sect->fragment = rdtrimc(&fname_c);
	    sect->level = level;
	    /*      printf(" ! adding para @ %p as sect %s, level %i\n", sect->para, sect->fragment, level); */

	    if (level > currentsect->level) {	/* case (3) */
		if (level > conf.leaf_level) {	/* same file */
		    assert(currentfile->is_leaf);
		    currentsect->child = sect;
		    sect->parent = currentsect;
		    sect->file = currentfile;
		    /*          printf("connected '%s' to existing file '%s' [I]\n", sect->fragment, currentfile->filename); */
		    currentsect = sect;
		} else {	/* new file */
		    xhtmlfile *file = xhtml_new_file(sect);
		    assert(!currentfile->is_leaf);
		    currentfile->child = file;
		    sect->file = file;
		    file->parent = currentfile;
		    /*          printf("connected '%s' to new file '%s' [I]\n", sect->fragment, file->filename); */
		    currentfile = file;
		    currentsect = sect;
		}
	    } else if (level >= currentsect->file->sections->level) {
		/* Case (1) or (2) *AND* still under the section that starts
		 * the current file.
		 *
		 * I'm not convinced that this couldn't be rolled in with the
		 * final else {} leg further down. It seems a lot of effort
		 * this way.
		 */
		if (level > conf.leaf_level) {	/* stick within the same file */
		    assert(currentfile->is_leaf);
		    sect->file = currentfile;
		    while (currentsect && currentsect->level > level &&
			   currentsect->file == currentsect->parent->file)
		    {
			currentsect = currentsect->parent;
		    }
		    assert(currentsect);
		    currentsect->next = sect;
		    assert(currentsect->level == sect->level);
		    sect->parent = currentsect->parent;
		    currentsect = sect;
		    /*          printf("connected '%s' to existing file '%s' [II]\n", sect->fragment, currentfile->filename); */
		} else {	/* new file */
		    xhtmlfile *file = xhtml_new_file(sect);
		    sect->file = file;
		    currentfile->next = file;
		    file->parent = currentfile->parent;
		    file->is_leaf = (level == conf.leaf_level);
		    file->sections = sect;
		    /*          printf("connected '%s' to new file '%s' [II]\n", sect->fragment, file->filename); */
		    currentfile = file;
		    currentsect = sect;
		}
	    } else {		/* Case (1) or (2) and we must move up the file tree first */
		/* this loop is now probably irrelevant - we know we can't connect
		 * to anything in the current file */
		while (currentsect && level < currentsect->level) {
		    currentsect = currentsect->parent;
		    if (currentsect) {
			/*            printf(" * up one level to '%s'\n", currentsect->fragment); */
		    } else {
			/*            printf(" * up one level (off top of current file)\n"); */
		    }
		}
		if (currentsect) {
		    /* I'm pretty sure this can now never fire */
		    assert(currentfile->is_leaf);
		    /*          printf("connected '%s' to existing file '%s' [III]\n", sect->fragment, currentfile->filename); */
		    sect->file = currentfile;
		    currentsect->next = sect;
		    currentsect = sect;
		} else {	/* find a file we can attach to */
		    while (currentfile && currentfile->sections
			   && level < currentfile->sections->level) {
			currentfile = currentfile->parent;
			if (currentfile) {
			    /*              printf(" * up one file level to '%s'\n", currentfile->filename); */
			} else {
			    /*              printf(" * up one file level (off top of tree)\n"); */
			}
		    }
		    if (currentfile) {	/* new file (we had to skip up a file to
					   get here, so we must be dealing with a
					   level no lower than the configured
					   leaf_level */
			xhtmlfile *file = xhtml_new_file(sect);
			currentfile->next = file;
			sect->file = file;
			file->parent = currentfile->parent;
			file->is_leaf = (level == conf.leaf_level);
			file->sections = sect;
			/*            printf("connected '%s' to new file '%s' [III]\n", sect->fragment, file->filename); */
			currentfile = file;
			currentsect = sect;
		    } else {
			fatal(err_whatever,
			      "Ran off the top trying to connect sibling: strange document.");
		    }
		}
	    }
	}
    }
    topsection = lastsection;	/* get correct end of the chain */
    xhtml_fixup_layout(topfile);	/* leaf files not at leaf level marked as such */
}

static void xhtml_do_index();
static void xhtml_do_file(xhtmlfile * file);
static void xhtml_do_top_file(xhtmlfile * file, paragraph * sourceform);
static void xhtml_do_paras(FILE * fp, paragraph * p);
static int xhtml_do_contents_limit(FILE * fp, xhtmlfile * file, int limit);
static int xhtml_do_contents_section_limit(FILE * fp,
					   xhtmlsection * section,
					   int limit);
static int xhtml_add_contents_entry(FILE * fp, xhtmlsection * section,
				    int limit);
static int xhtml_do_contents(FILE * fp, xhtmlfile * file);
static int xhtml_do_naked_contents(FILE * fp, xhtmlfile * file);
static void xhtml_do_sections(FILE * fp, xhtmlsection * sections);

/*
 * Do all the files in this structure.
 */
static void xhtml_do_files(xhtmlfile * file)
{
    xhtml_do_file(file);
    if (file->child)
	xhtml_do_files(file->child);
    if (file->next)
	xhtml_do_files(file->next);
}

/*
 * Free up all memory used by the file tree from 'xfile' downwards
 */
static void xhtml_free_file(xhtmlfile * xfile)
{
    if (xfile == NULL) {
	return;
    }

    if (xfile->filename) {
	sfree(xfile->filename);
    }
    xhtml_free_file(xfile->child);
    xhtml_free_file(xfile->next);
    sfree(xfile);
}

/*
 * Main function.
 */
void
xhtml_backend(paragraph * sourceform, keywordlist * in_keywords,
	      indexdata * in_idx)
{
/*  int i;*/
    indexentry *ientry;
    int ti;
    xhtmlsection *xsect;

    sourceparas = sourceform;
    conf = xhtml_configure(sourceform);
    keywords = in_keywords;
    idx = in_idx;

    /* Clear up the index entries backend data pointers */
    for (ti = 0;
	 (ientry = (indexentry *) index234(idx->entries, ti)) != NULL;
	 ti++) {
	ientry->backend_data = NULL;
    }

    xhtml_ponder_layout(sourceform);

    /* old system ... (writes to *.alt, but gets some stuff wrong and is ugly) */
/*  xhtml_level_0(sourceform);
  for (i=1; i<=conf.leaf_level; i++)
  {
    xhtml_level(sourceform, i);
  }*/

    /* new system ... (writes to *.html, but isn't fully trusted) */
    xhtml_do_top_file(topfile, sourceform);
    assert(!topfile->next);	/* shouldn't have a sibling at all */
    if (topfile->child) {
	xhtml_do_files(topfile->child);
	xhtml_do_index();
    }

    /* release file, section, index data structures */
    xsect = topsection;
    while (xsect) {
	xhtmlsection *tmp = xsect->chain;
	if (xsect->fragment) {
	    sfree(xsect->fragment);
	}
	sfree(xsect);
	xsect = tmp;
    }
    xhtml_free_file(topfile);
    for (ti = 0;
	 (ientry = (indexentry *) index234(idx->entries, ti)) != NULL;
	 ti++) {
	if (ientry->backend_data != NULL) {
	    xhtmlindex *xi = (xhtmlindex *) ientry->backend_data;
	    if (xi->sections != NULL) {
		sfree(xi->sections);
	    }
	    sfree(xi);
	}
	ientry->backend_data = NULL;
    }
    {
	int i;
	sfree(conf.fchapter.number_suffix);
	for (i = 0; i < conf.nfsect; i++)
	    sfree(conf.fsect[i].number_suffix);
	sfree(conf.fsect);
    }
}

static int xhtml_para_level(paragraph * p)
{
    switch (p->type) {
    case para_Title:
	return 0;
	break;
    case para_UnnumberedChapter:
    case para_Chapter:
    case para_Appendix:
	return 1;
	break;
/*  case para_BiblioCited:
    return 2;
    break;*/
    case para_Heading:
    case para_Subsect:
	return p->aux + 2;
	break;
    default:
	return -1;
	break;
    }
}

static char *xhtml_index_filename = "IndexPage.html";

/* Output the nav links for the current file.
 * file == NULL means we're doing the index
 */
static void xhtml_donavlinks(FILE * fp, xhtmlfile * file)
{
    xhtmlfile *xhtml_next_file = NULL;
    fprintf(fp, "<p");
    if (conf.nav_attrs != NULL) {
	fprintf(fp, " %ls>", conf.nav_attrs);
    } else {
	fprintf(fp, ">");
    }
    if (xhtml_last_file == NULL) {
	fprintf(fp, "Previous | ");
    } else {
	fprintf(fp, "<a href='%s'>Previous</a> | ",
		xhtml_last_file->filename);
    }
    fprintf(fp, "<a href='Contents.html'>Contents</a> | ");
    if (file != NULL) {		/* otherwise we're doing nav links for the index */
	if (xhtml_next_file == NULL)
	    xhtml_next_file = file->child;
	if (xhtml_next_file == NULL)
	    xhtml_next_file = file->next;
	if (xhtml_next_file == NULL)
	    xhtml_next_file = file->parent->next;
    }
    if (xhtml_next_file == NULL) {
	if (file == NULL) {	/* index, so no next file */
	    fprintf(fp, "Next	");
	} else {
	    fprintf(fp, "<a href='%s'>Next</a>", xhtml_index_filename);
	}
    } else {
	fprintf(fp, "<a href='%s'>Next</a>", xhtml_next_file->filename);
    }
    fprintf(fp, "</p>\n");
}

/* Write out the index file */
static void xhtml_do_index_body(FILE * fp)
{
    indexentry *y;
    int ti;

    if (count234(idx->entries) == 0)
	return;			/* don't write anything at all */

    fprintf(fp, "<dl>\n");
    /* iterate over idx->entries using the tree functions and display everything */
    for (ti = 0; (y = (indexentry *) index234(idx->entries, ti)) != NULL;
	 ti++) {
	if (y->backend_data) {
	    int i;
	    xhtmlindex *xi;

	    fprintf(fp, "<dt>");
	    xhtml_para(fp, y->text);
	    fprintf(fp, "</dt>\n<dd>");

	    xi = (xhtmlindex *) y->backend_data;
	    for (i = 0; i < xi->nsection; i++) {
		xhtmlsection *sect = xi->sections[i];
		if (sect) {
		    fprintf(fp, "<a href='%s#%s'>", sect->file->filename,sect->fragment);
		    if (sect->para->kwtext) {
			xhtml_para(fp, sect->para->kwtext);
		    } else if (sect->para->words) {
			xhtml_para(fp, sect->para->words);
		    }
		    fprintf(fp, "</a>");
		    if (i + 1 < xi->nsection) {
			fprintf(fp, ", ");
		    }
		}
	    }
	    fprintf(fp, "</dd>\n");
	}
    }
    fprintf(fp, "</dl>\n");
}
static void xhtml_do_index()
{
    word temp_word =
	{ NULL, NULL, word_Normal, 0, 0, L"Index", {NULL, 0, 0} };
    FILE *fp = fopen(xhtml_index_filename, "w");

    if (fp == NULL)
	fatal(err_cantopenw, xhtml_index_filename);
    xhtml_doheader(fp, &temp_word);
    xhtml_donavlinks(fp, NULL);

    xhtml_do_index_body(fp);

    xhtml_donavlinks(fp, NULL);
    xhtml_dofooter(fp);
    fclose(fp);
}

/* Output the given file. This includes whatever contents at beginning and end, etc. etc. */
static void xhtml_do_file(xhtmlfile * file)
{
    FILE *fp = fopen(file->filename, "w");
    if (fp == NULL)
	fatal(err_cantopenw, file->filename);

    if (file->sections->para->words) {
	xhtml_doheader(fp, file->sections->para->words);
    } else if (file->sections->para->kwtext) {
	xhtml_doheader(fp, file->sections->para->kwtext);
    } else {
	xhtml_doheader(fp, NULL);
    }

    xhtml_donavlinks(fp, file);

    if (file->is_leaf && conf.leaf_contains_contents &&
	xhtml_do_contents(NULL, file) >= conf.leaf_smallest_contents)
	xhtml_do_contents(fp, file);
    xhtml_do_sections(fp, file->sections);
    if (!file->is_leaf)
	xhtml_do_naked_contents(fp, file);

    xhtml_donavlinks(fp, file);

    xhtml_dofooter(fp);
    fclose(fp);

    xhtml_last_file = file;
}

/* Output the top-level file. */
static void xhtml_do_top_file(xhtmlfile * file, paragraph * sourceform)
{
    paragraph *p;
    int done = FALSE;
    FILE *fp = fopen(file->filename, "w");
    if (fp == NULL)
	fatal(err_cantopenw, file->filename);

    /* Do the title -- only one allowed */
    for (p = sourceform; p && !done; p = p->next) {
	if (p->type == para_Title) {
	    xhtml_doheader(fp, p->words);
	    done = TRUE;
	}
    }
    if (!done)
	xhtml_doheader(fp, NULL /* Eek! */ );

    /*
     * Display the title.
     */
    for (p = sourceform; p; p = p->next) {
	if (p->type == para_Title) {
	    xhtml_heading(fp, p);
	    break;
	}
    }

    /* Do the preamble and copyright */
    for (p = sourceform; p; p = p->next) {
	if (p->type == para_Preamble) {
	    fprintf(fp, "<p>");
	    xhtml_para(fp, p->words);
	    fprintf(fp, "</p>\n");
	}
    }
    for (p = sourceform; p; p = p->next) {
	if (p->type == para_Copyright) {
	    fprintf(fp, "<p>");
	    xhtml_para(fp, p->words);
	    fprintf(fp, "</p>\n");
	}
    }

    xhtml_do_contents(fp, file);
    xhtml_do_sections(fp, file->sections);

    /*
     * Put the index in the top file if we're in single-file mode
     * (leaf-level 0).
     */
    if (conf.leaf_level == 0 && count234(idx->entries) > 0) {
	fprintf(fp, "<a name=\"index\"></a><h1>Index</h1>\n");
	xhtml_do_index_body(fp);
    }

    xhtml_dofooter(fp);
    fclose(fp);
}

/* Convert a Unicode string to an ASCII one. '?' is
 * used for unmappable characters.
 */
static void xhtml_utostr(wchar_t * in, char **out)
{
    int l = ustrlen(in);
    int i;
    *out = smalloc(l + 1);
    for (i = 0; i < l; i++) {
	if (in[i] >= 32 && in[i] <= 126)
	    (*out)[i] = (char) in[i];
	else
	    (*out)[i] = '?';
    }
    (*out)[i] = 0;
}

/*
 * Write contents for the given file, and subfiles, down to
 * the appropriate contents depth. Returns the number of
 * entries written.
 */
static int xhtml_do_contents(FILE * fp, xhtmlfile * file)
{
    int level, limit, start_level, count = 0;
    if (!file)
	return 0;

    level = (file->sections) ? (file->sections->level) : (0);
    limit = conf.contents_depth[(level > 5) ? (5) : (level)];
    start_level = (file->is_leaf) ? (level - 1) : (level);
    last_level = start_level;

    count += xhtml_do_contents_section_limit(fp, file->sections, limit);
    count += xhtml_do_contents_limit(fp, file->child, limit);
    if (fp != NULL) {
	while (last_level > start_level) {
	    last_level--;
	    fprintf(fp, "</ul>\n");
	}
    }
    return count;
}

/* As above, but doesn't do anything in the current file */
static int xhtml_do_naked_contents(FILE * fp, xhtmlfile * file)
{
    int level, limit, start_level, count = 0;
    if (!file)
	return 0;

    level = (file->sections) ? (file->sections->level) : (0);
    limit = conf.contents_depth[(level > 5) ? (5) : (level)];
    start_level = (file->is_leaf) ? (level - 1) : (level);
    last_level = start_level;

    count = xhtml_do_contents_limit(fp, file->child, limit);
    if (fp != NULL) {
	while (last_level > start_level) {
	    last_level--;
	    fprintf(fp, "</ul>\n");
	}
    }
    return count;
}

/*
 * Write contents for the given file, children, and siblings, down to
 * given limit contents depth.
 */
static int xhtml_do_contents_limit(FILE * fp, xhtmlfile * file, int limit)
{
    int count = 0;
    while (file) {
	count +=
	    xhtml_do_contents_section_limit(fp, file->sections, limit);
	count += xhtml_do_contents_limit(fp, file->child, limit);
	file = file->next;
    }
    return count;
}

/*
 * Write contents entries for the given section tree, down to the
 * limit contents depth.
 */
static int
xhtml_do_contents_section_deep_limit(FILE * fp, xhtmlsection * section,
				     int limit)
{
    int count = 0;
    while (section) {
	if (!xhtml_add_contents_entry(fp, section, limit))
	    return 0;
	else
	    count++;
	count +=
	    xhtml_do_contents_section_deep_limit(fp, section->child,
						 limit);
	section = section->next;
    }
    return count;
}

/*
 * Write contents entries for the given section tree, down to the
 * limit contents depth.
 */
static int
xhtml_do_contents_section_limit(FILE * fp, xhtmlsection * section,
				int limit)
{
    int count = 0;
    if (!section)
	return 0;
    xhtml_add_contents_entry(fp, section, limit);
    count = 1;
    count +=
	xhtml_do_contents_section_deep_limit(fp, section->child, limit);
    /*  section=section->child;
       while (section && xhtml_add_contents_entry(fp, section, limit)) {
       section = section->next;
       } */
    return count;
}

/*
 * Add a section entry, unless we're exceeding the limit, in which
 * case return FALSE (otherwise return TRUE).
 */
static int
xhtml_add_contents_entry(FILE * fp, xhtmlsection * section, int limit)
{
    if (!section || section->level > limit)
	return FALSE;
    if (fp == NULL || section->level < 0)
	return TRUE;
    while (last_level > section->level) {
	last_level--;
	fprintf(fp, "</ul>\n");
    }
    while (last_level < section->level) {
	last_level++;
	fprintf(fp, "<ul>\n");
    }
    fprintf(fp, "<li>");
    fprintf(fp, "<a href=\"%s#%s\">", section->file->filename,section->fragment);
    if ((section->para->type!=para_Heading&&section->para->type!=para_Subsect)||(section->para->kwtext&&!section->para->words)) {
	    xhtml_para(fp, section->para->kwtext);
        if (section->para->words)
            fprintf(fp, ": ");
    }
    if (section->para->words) {
	    xhtml_para(fp, section->para->words);
    }
    fprintf(fp, "</a></li>\n");
    return TRUE;
}

/*
 * Write all the sections in this file. Do all paragraphs in this section, then all
 * children (recursively), then go on to the next one (tail recursively).
 */
static void xhtml_do_sections(FILE * fp, xhtmlsection * sections)
{
    while (sections) {
	currentsection = sections;
	xhtml_do_paras(fp, sections->para);
	xhtml_do_sections(fp, sections->child);
	sections = sections->next;
    }
}

/* Write this list of paragraphs. Close off all lists at the end. */
static void xhtml_do_paras(FILE * fp, paragraph * p)
{
    int last_type = -1, first = TRUE;
    if (!p)
	return;

/*  for (; p && (xhtml_para_level(p)>limit || xhtml_para_level(p)==-1 || first); p=p->next) {*/
    for (; p && (xhtml_para_level(p) == -1 || first); p = p->next) {
	first = FALSE;
	switch (p->type) {
	    /*
	     * Things we ignore because we've already processed them or
	     * aren't going to touch them in this pass.
	     */
	case para_IM:
	case para_BR:
	case para_Biblio:	/* only touch BiblioCited */
	case para_VersionID:
	case para_Copyright:
	case para_Preamble:
	case para_NoCite:
	case para_Title:
	    break;

	    /*
	     * Chapter titles.
	     */
	case para_Chapter:
	case para_Appendix:
	case para_UnnumberedChapter:
	    xhtml_heading(fp, p);
	    break;

	case para_Heading:
	case para_Subsect:
	    xhtml_heading(fp, p);
	    break;

	case para_Rule:
	    fprintf(fp, "\n<hr />\n");
	    break;

	case para_Normal:
	    fprintf(fp, "\n<p>");
	    xhtml_para(fp, p->words);
	    fprintf(fp, "</p>\n");
	    break;

	case para_Bullet:
	case para_NumberedList:
	case para_BiblioCited:
	    if (last_type != p->type) {
		/* start up list if necessary */
		if (p->type == para_Bullet) {
		    fprintf(fp, "<ul>\n");
		} else if (p->type == para_NumberedList) {
		    fprintf(fp, "<ol>\n");
		} else if (p->type == para_BiblioCited) {
		    fprintf(fp, "<dl>\n");
		}
	    }
	    if (p->type == para_Bullet || p->type == para_NumberedList)
		fprintf(fp, "<li>");
	    else if (p->type == para_BiblioCited) {
		fprintf(fp, "<dt>");
		xhtml_para(fp, p->kwtext);
		fprintf(fp, "</dt>\n<dd>");
	    }
	    xhtml_para(fp, p->words);
	    if (p->type == para_BiblioCited) {
		fprintf(fp, "</dd>\n");
	    } else if (p->type == para_Bullet
		       || p->type == para_NumberedList) {
		fprintf(fp, "</li>");
	    }
	    if (p->type == para_Bullet || p->type == para_NumberedList
		|| p->type == para_BiblioCited)
		/* close off list if necessary */
	    {
		paragraph *p2 = p->next;
		int close_off = FALSE;
/*          if (p2 && (xhtml_para_level(p2)>limit || xhtml_para_level(p2)==-1)) {*/
		if (p2 && xhtml_para_level(p2) == -1) {
		    if (p2->type != p->type)
			close_off = TRUE;
		} else {
		    close_off = TRUE;
		}
		if (close_off) {
		    if (p->type == para_Bullet) {
			fprintf(fp, "</ul>\n");
		    } else if (p->type == para_NumberedList) {
			fprintf(fp, "</ol>\n");
		    } else if (p->type == para_BiblioCited) {
			fprintf(fp, "</dl>\n");
		    }
		}
	    }
	    break;

	case para_Code:
	    xhtml_codepara(fp, p->words);
	    break;
	}
	last_type = p->type;
    }
}

/*
 * Output a header for this XHTML file.
 */
static void xhtml_doheader(FILE * fp, word * title)
{
    fprintf(fp,
	    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n");
    fprintf(fp,
	    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n");
    fprintf(fp,
	    "<html xmlns='http://www.w3.org/1999/xhtml'>\n\n<head>\n<title>");
    if (title == NULL)
	fprintf(fp, "Documentation");
    else
	xhtml_para(fp, title);
    fprintf(fp, "</title>\n");
    fprintf(fp,
	    "<meta name=\"generator\" content=\"Halibut %s xhtml-backend\" />\n", version);
    if (conf.author)
	fprintf(fp, "<meta name=\"author\" content=\"%ls\" />\n",
		conf.author);
    if (conf.description)
	fprintf(fp, "<meta name=\"description\" content=\"%ls\" />\n",
		conf.description);
    if (conf.head_end)
	fprintf(fp, "%ls\n", conf.head_end);
    fprintf(fp, "</head>\n\n");
    if (conf.body)
	fprintf(fp, "%ls\n", conf.body);
    else
	fprintf(fp, "<body>\n");
    if (conf.body_start)
	fprintf(fp, "%ls\n", conf.body_start);
}

/*
 * Output a footer for this XHTML file.
 */
static void xhtml_dofooter(FILE * fp)
{
    fprintf(fp, "\n<hr />\n\n");
    if (conf.body_end)
	fprintf(fp, "%ls\n", conf.body_end);
    if (!conf.suppress_address) {
	fprintf(fp, "<address>\n");
	if (conf.address_start)
	    fprintf(fp, "%ls\n", conf.address_start);
	/* Do the version ID */
	if (conf.include_version_id) {
	    paragraph *p;
	    int started = 0;
	    for (p = sourceparas; p; p = p->next)
		if (p->type == para_VersionID) {
		    xhtml_versionid(fp, p->words, started);
		    started = 1;
		}
	}
	if (conf.address_end)
	    fprintf(fp, "%ls\n", conf.address_end);
	fprintf(fp, "</address>\n");
    }
    fprintf(fp, "</body>\n\n</html>\n");
}

/*
 * Output the versionid paragraph. Typically this is a version control
 * ID string (such as $Id...$ in RCS).
 */
static void xhtml_versionid(FILE * fp, word * text, int started)
{
    rdstringc t = { 0, 0, NULL };

    rdaddc(&t, '[');		/* FIXME: configurability */
    xhtml_rdaddwc(&t, text, NULL);
    rdaddc(&t, ']');		/* FIXME: configurability */

    if (started)
	fprintf(fp, "<br>\n");
    fprintf(fp, "%s\n", t.text);
    sfree(t.text);
}

/* Is this an XHTML reserved character? */
static int xhtml_reservedchar(int c)
{
    if (c == '&' || c == '<' || c == '>' || c == '"')
	return TRUE;
    else
	return FALSE;
}

/*
 * Convert a wide string into valid XHTML: Anything outside ASCII will
 * be fixed up as an entity. Currently we don't worry about constraining the
 * encoded character set, which we should probably do at some point (we can
 * still fix up and return FALSE - see the last comment here). We also don't
 * currently
 *
 * Because this is only used for words, spaces are HARD spaces (any other
 * spaces will be word_Whitespace not word_Normal). So they become &nbsp;
 * Unless hard_spaces is FALSE, of course (code paragraphs break the above
 * rule).
 *
 * If `result' is non-NULL, mallocs the resulting string and stores a pointer to
 * it in `*result'. If `result' is NULL, merely checks whether all
 * characters in the string are feasible.
 *
 * Return is nonzero if all characters are OK. If not all
 * characters are OK but `result' is non-NULL, a result _will_
 * still be generated!
 */
static int xhtml_convert(wchar_t * s, char **result, int hard_spaces)
{
    int doing = (result != 0);
    int ok = TRUE;
    char *p = NULL;
    int plen = 0, psize = 0;

    for (; *s; s++) {
	wchar_t c = *s;

#define ensure_size(i) if (i>=psize) { psize = i+256; p = resize(p, psize); }

	if (((c == 32 && !hard_spaces)
	     || (c > 32 && c <= 126 && !xhtml_reservedchar(c)))) {
	    /* Char is OK. */
	    if (doing) {
		ensure_size(plen);
		p[plen++] = (char) c;
	    }
	} else {
	    /* Char needs fixing up. */
	    /* ok = FALSE; -- currently we never return FALSE; we
	     * might want to when considering a character set for the
	     * encoded document.
	     */
	    if (doing) {
		if (c == 32) {	/* a space in a word is a hard space */
		    ensure_size(plen + 7);	/* includes space for the NUL, which is subsequently stomped on */
		    sprintf(p + plen, "&nbsp;");
		    plen += 6;
		} else {
		    switch (c) {
		    case '&':
			ensure_size(plen + 6);	/* includes space for the NUL, which is subsequently stomped on */
			plen += sprintf(p + plen, "&amp;");
			break;
		    case '"':
			ensure_size(plen + 7);	/* includes space for the NUL, which is subsequently stomped on */
			plen += sprintf(p + plen, "&quot;");
			break;
		    case '<':
			if (plen > 1 && *(s - 1) == '\\'
			    && *(s - 2) == '\\') {
			    ensure_size(--plen);
			    p[plen - 1] = (char) c;
			    p[plen] = 0;
			} else {
			    ensure_size(plen + 5);	/* includes space for the NUL, which is subsequently stomped on */
			    plen += sprintf(p + plen, "&lt;");
			}
			break;
		    case '>':
			if (plen > 1 && *(s - 1) == '\\'
			    && *(s - 2) == '\\') {
			    ensure_size(--plen);
			    p[plen - 1] = (char) c;
			    p[plen] = 0;
			} else {
			    ensure_size(plen + 5);	/* includes space for the NUL, which is subsequently stomped on */
			    plen += sprintf(p + plen, "&gt;");
			}
			break;
		    default:
			ensure_size(plen + 8);	/* includes space for the NUL, which is subsequently stomped on */
			plen += sprintf(p + plen, "&#%04i;", (int) c);
			break;
		    }
		}
	    }
	}
    }
    if (doing) {
	p = resize(p, plen + 1);
	p[plen] = '\0';
	*result = p;
    }

    return ok;
}

/*
 * This formats the given words as XHTML.
 */
static void xhtml_rdaddwc(rdstringc * rs, word * text, word * end)
{
    char *c;
    keyword *kwl;
    xhtmlsection *sect;
    indextag *itag;
    int ti;

    for (; text && text != end; text = text->next) {
	switch (text->type) {
	case word_HyperLink:
	    xhtml_utostr(text->text, &c);
	    rdaddsc(rs, "<a href=\"");
	    rdaddsc(rs, c);
	    rdaddsc(rs, "\">");
	    sfree(c);
	    break;

	case word_UpperXref:
	case word_LowerXref:
	    kwl = kw_lookup(keywords, text->text);
	    if (kwl) {
		sect = xhtml_find_section(kwl->para);
		if (sect) {
		    rdaddsc(rs, "<a href=\"");
		    rdaddsc(rs, sect->file->filename);
		    rdaddc(rs, '#');
		    rdaddsc(rs, sect->fragment);
		    rdaddsc(rs, "\">");
		} else {
		    rdaddsc(rs,
			    "<a href=\"Apologies.html\"><!-- probably a bibliography cross reference -->");
		    error(err_whatever,
			  "Couldn't locate cross-reference! (Probably a bibliography entry.)");
		}
	    } else {
		rdaddsc(rs,
			"<a href=\"Apologies.html\"><!-- unknown cross-reference -->");
		error(err_whatever,
		      "Couldn't locate cross-reference! (Wasn't in source file.)");
	    }
	    break;

	case word_IndexRef:	/* in theory we could make an index target here */
/*        rdaddsc(rs, "<a name=\"idx-");
        xhtml_utostr(text->text, &c);
        rdaddsc(rs, c);
        sfree(c);
        rdaddsc(rs, "\"></a>");*/
	    /* what we _do_ need to do is to fix up the backend data
	     * for any indexentry this points to.
	     */
	    for (ti = 0;
		 (itag = (indextag *) index234(idx->tags, ti)) != NULL;
		 ti++) {
		/* FIXME: really ustricmp() and not ustrcmp()? */
		if (ustricmp(itag->name, text->text) == 0) {
		    break;
		}
	    }
	    if (itag != NULL) {
		if (itag->refs != NULL) {
		    int i;
		    for (i = 0; i < itag->nrefs; i++) {
			xhtmlindex *idx_ref;
			indexentry *ientry;

			ientry = itag->refs[i];
			if (ientry->backend_data == NULL) {
			    idx_ref =
				(xhtmlindex *) smalloc(sizeof(xhtmlindex));
			    if (idx_ref == NULL)
				fatal(err_nomemory);
			    idx_ref->nsection = 0;
			    idx_ref->size = 4;
			    idx_ref->sections =
				(xhtmlsection **) smalloc(idx_ref->size *
							  sizeof
							  (xhtmlsection
							   *));
			    if (idx_ref->sections == NULL)
				fatal(err_nomemory);
			    ientry->backend_data = idx_ref;
			} else {
			    idx_ref = ientry->backend_data;
			    if (idx_ref->nsection + 1 > idx_ref->size) {
				int new_size = idx_ref->size * 2;
				idx_ref->sections =
				    srealloc(idx_ref->sections,
					     new_size *
					     sizeof(xhtmlsection));
				if (idx_ref->sections == NULL) {
				    fatal(err_nomemory);
				}
				idx_ref->size = new_size;
			    }
			}
			idx_ref->sections[idx_ref->nsection++] =
			    currentsection;
#if 0
#endif
		    }
		} else {
		    fatal(err_whatever, "Index tag had no entries!");
		}
	    } else {
		fprintf(stderr, "Looking for index entry '%ls'\n",
			text->text);
		fatal(err_whatever,
		      "Couldn't locate index entry! (Wasn't in index.)");
	    }
	    break;

	case word_HyperEnd:
	case word_XrefEnd:
	    rdaddsc(rs, "</a>");
	    break;

	case word_Normal:
	case word_Emph:
	case word_Code:
	case word_WeakCode:
	case word_WhiteSpace:
	case word_EmphSpace:
	case word_CodeSpace:
	case word_WkCodeSpace:
	case word_Quote:
	case word_EmphQuote:
	case word_CodeQuote:
	case word_WkCodeQuote:
	    assert(text->type != word_CodeQuote &&
		   text->type != word_WkCodeQuote);
	    if (towordstyle(text->type) == word_Emph &&
		(attraux(text->aux) == attr_First ||
		 attraux(text->aux) == attr_Only))
		rdaddsc(rs, "<em>");
	    else if ((towordstyle(text->type) == word_Code
		      || towordstyle(text->type) == word_WeakCode)
		     && (attraux(text->aux) == attr_First
			 || attraux(text->aux) == attr_Only))
		rdaddsc(rs, "<code>");

	    if (removeattr(text->type) == word_Normal) {
		static int dont_convert = 0;
		if (dont_convert) {
		    char buf[2] = " ";
		    dont_convert = 0;
		    wchar_t *s = text->text;
		    for (; *s; s++) {
			buf[0] = (char) *s;
			rdaddsc(rs, buf);
		    }
		    buf[0] = 0;
		    rdaddsc(rs, buf);
		} else {
		    if (*text->text == '\\' && text->next
			&& text->next->text && (*text->next->text == '&'
						|| *text->next->text == '<'
						|| *text->next->text == '>'
						|| *text->next->text ==
						'"'))
			dont_convert = 1;
		    else {
			if (xhtml_convert(text->text, &c, TRUE))	/* spaces in the word are hard */
			    rdaddsc(rs, c);
			else
			    xhtml_rdaddwc(rs, text->alt, NULL);
			sfree(c);
		    }
		}
	    } else if (removeattr(text->type) == word_WhiteSpace) {
		rdaddc(rs, ' ');
	    } else if (removeattr(text->type) == word_Quote) {
		rdaddsc(rs, "&quot;");
	    }

	    if (towordstyle(text->type) == word_Emph &&
		(attraux(text->aux) == attr_Last ||
		 attraux(text->aux) == attr_Only))
		rdaddsc(rs, "</em>");
	    else if ((towordstyle(text->type) == word_Code
		      || towordstyle(text->type) == word_WeakCode)
		     && (attraux(text->aux) == attr_Last
			 || attraux(text->aux) == attr_Only))
		rdaddsc(rs, "</code>");
	    break;
	}
    }
}

/* Output a heading, formatted as XHTML.
 */
static void xhtml_heading(FILE * fp, paragraph * p)
{
    rdstringc t = { 0, 0, NULL };
    word *tprefix = p->kwtext;
    word *nprefix = p->kwtext2;
    word *text = p->words;
    int level = xhtml_para_level(p);
    xhtmlsection *sect = xhtml_find_section(p);
    xhtmlheadfmt *fmt;
    char *fragment;
    if (sect) {
	fragment = sect->fragment;
    } else {
	if (p->type == para_Title)
	    fragment = "title";
	else {
	    fragment = "";	/* FIXME: what else can we do? */
	    error(err_whatever,
		  "Couldn't locate heading cross-reference!");
	}
    }

    if (p->type == para_Title)
	fmt = NULL;
    else if (level == 1)
	fmt = &conf.fchapter;
    else if (level - 1 < conf.nfsect)
	fmt = &conf.fsect[level - 1];
    else
	fmt = &conf.fsect[conf.nfsect - 1];

    if (fmt && fmt->just_numbers && nprefix) {
	xhtml_rdaddwc(&t, nprefix, NULL);
	if (fmt) {
	    char *c;
	    if (xhtml_convert(fmt->number_suffix, &c, FALSE)) {
		rdaddsc(&t, c);
		sfree(c);
	    }
	}
    } else if (fmt && !fmt->just_numbers && tprefix) {
	xhtml_rdaddwc(&t, tprefix, NULL);
	if (fmt) {
	    char *c;
	    if (xhtml_convert(fmt->number_suffix, &c, FALSE)) {
		rdaddsc(&t, c);
		sfree(c);
	    }
	}
    }
    xhtml_rdaddwc(&t, text, NULL);
    /*
     * If we're outputting in single-file mode, we need to lower
     * the level of each heading by one, because the overall
     * document title will be sitting right at the top as an <h1>
     * and so chapters and sections should start at <h2>.
     * 
     * Even if not, the document title will come back from
     * xhtml_para_level() as level zero, so we must increment that
     * no matter what leaf_level is set to.
     */
    if (conf.leaf_level == 0 || level == 0)
	level++;
    fprintf(fp, "<a name=\"%s\"></a><h%i>%s</h%i>\n", fragment, level,
	    t.text, level);
    sfree(t.text);
}

/* Output a paragraph. Styles are handled by xhtml_rdaddwc().
 * This looks pretty simple; I may have missed something ...
 */
static void xhtml_para(FILE * fp, word * text)
{
    rdstringc out = { 0, 0, NULL };
    xhtml_rdaddwc(&out, text, NULL);
    fprintf(fp, "%s", out.text);
    sfree(out.text);
}

/* Output a code paragraph. I'm treating this as preformatted, which
 * may not be entirely correct. See xhtml_para() for my worries about
 * this being overly-simple; however I think that most of the complexity
 * of the text backend came entirely out of word wrapping anyway.
 */
static void xhtml_codepara(FILE * fp, word * text)
{
    fprintf(fp, "<pre>");
    for (; text; text = text->next)
	if (text->type == word_WeakCode) {
	    char *c;
	    xhtml_convert(text->text, &c, FALSE);
	    fprintf(fp, "%s\n", c);
	    sfree(c);
	}
    fprintf(fp, "</pre>\n");
}
