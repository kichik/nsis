/*
 * text backend for Halibut
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "halibut.h"

typedef enum
{ LEFT, LEFTPLUS, CENTRE }
alignment;
typedef struct
{
  alignment align;
  int just_numbers;
  wchar_t underline;
  wchar_t *number_suffix;
}
alignstruct;

typedef struct
{
  int indent, indent_code;
  int listindentbefore, listindentafter;
  int width;
  alignstruct atitle, achapter, *asect;
  int nasect;
  int include_version_id;
  int indent_preambles;
  word bullet;
}
textconfig;

static int text_convert (wchar_t *, char **);

static void text_heading (FILE *, word *, word *, word *, alignstruct, int,
                          int);
static void text_rule (FILE *, int, int);
static void text_para (FILE *, word *, char *, word *, int, int, int);
static void text_codepara (FILE *, word *, int, int);
static void text_versionid (FILE *, word *);

static alignment
utoalign (wchar_t * p)
{
  if (!ustricmp (p, L"centre") || !ustricmp (p, L"center"))
    return CENTRE;
  if (!ustricmp (p, L"leftplus"))
    return LEFTPLUS;
  return LEFT;
}

static textconfig
text_configure (paragraph * source)
{
  textconfig ret;

  /*
   * Non-negotiables.
   */
  ret.bullet.next = NULL;
  ret.bullet.alt = NULL;
  ret.bullet.type = word_Normal;
  ret.atitle.just_numbers = FALSE;      /* ignored */

  /*
   * Defaults.
   */
  ret.indent = 7;
  ret.indent_code = 2;
  ret.listindentbefore = 1;
  ret.listindentafter = 3;
  ret.width = 68;
  ret.atitle.align = CENTRE;
  ret.atitle.underline = L'=';
  ret.achapter.align = LEFT;
  ret.achapter.just_numbers = FALSE;
  ret.achapter.number_suffix = ustrdup (L": ");
  ret.achapter.underline = L'-';
  ret.nasect = 1;
  ret.asect = mknewa (alignstruct, ret.nasect);
  ret.asect[0].align = LEFTPLUS;
  ret.asect[0].just_numbers = TRUE;
  ret.asect[0].number_suffix = ustrdup (L" ");
  ret.asect[0].underline = L'\0';
  ret.include_version_id = TRUE;
  ret.indent_preambles = FALSE;
  ret.bullet.text = ustrdup (L"-");

  for (; source; source = source->next)
    {
      if (source->type == para_Config)
        {
          if (!ustricmp (source->keyword, L"text-indent"))
            {
              ret.indent = utoi (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-indent-code"))
            {
              ret.indent_code = utoi (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-width"))
            {
              ret.width = utoi (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-list-indent"))
            {
              ret.listindentbefore = utoi (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-listitem-indent"))
            {
              ret.listindentafter = utoi (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-chapter-align"))
            {
              ret.achapter.align = utoalign (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-chapter-underline"))
            {
              ret.achapter.underline = *uadv (source->keyword);
            }
          else if (!ustricmp (source->keyword, L"text-chapter-numeric"))
            {
              ret.achapter.just_numbers = utob (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-chapter-suffix"))
            {
              ret.achapter.number_suffix = ustrdup (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-section-align"))
            {
              wchar_t *p = uadv (source->keyword);
              int n = 0;
              if (uisdigit (*p))
                {
                  n = utoi (p);
                  p = uadv (p);
                }
              if (n >= ret.nasect)
                {
                  int i;
                  ret.asect = resize (ret.asect, n + 1);
                  for (i = ret.nasect; i <= n; i++)
                    ret.asect[i] = ret.asect[ret.nasect - 1];
                  ret.nasect = n + 1;
                }
              ret.asect[n].align = utoalign (p);
            }
          else if (!ustricmp (source->keyword, L"text-section-underline"))
            {
              wchar_t *p = uadv (source->keyword);
              int n = 0;
              if (uisdigit (*p))
                {
                  n = utoi (p);
                  p = uadv (p);
                }
              if (n >= ret.nasect)
                {
                  int i;
                  ret.asect = resize (ret.asect, n + 1);
                  for (i = ret.nasect; i <= n; i++)
                    ret.asect[i] = ret.asect[ret.nasect - 1];
                  ret.nasect = n + 1;
                }
              ret.asect[n].underline = *p;
            }
          else if (!ustricmp (source->keyword, L"text-section-numeric"))
            {
              wchar_t *p = uadv (source->keyword);
              int n = 0;
              if (uisdigit (*p))
                {
                  n = utoi (p);
                  p = uadv (p);
                }
              if (n >= ret.nasect)
                {
                  int i;
                  ret.asect = resize (ret.asect, n + 1);
                  for (i = ret.nasect; i <= n; i++)
                    ret.asect[i] = ret.asect[ret.nasect - 1];
                  ret.nasect = n + 1;
                }
              ret.asect[n].just_numbers = utob (p);
            }
          else if (!ustricmp (source->keyword, L"text-section-suffix"))
            {
              wchar_t *p = uadv (source->keyword);
              int n = 0;
              if (uisdigit (*p))
                {
                  n = utoi (p);
                  p = uadv (p);
                }
              if (n >= ret.nasect)
                {
                  int i;
                  ret.asect = resize (ret.asect, n + 1);
                  for (i = ret.nasect; i <= n; i++)
                    ret.asect[i] = ret.asect[ret.nasect - 1];
                  ret.nasect = n + 1;
                }
              ret.asect[n].number_suffix = ustrdup (p);
            }
          else if (!ustricmp (source->keyword, L"text-title-align"))
            {
              ret.atitle.align = utoalign (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-title-underline"))
            {
              ret.atitle.underline = *uadv (source->keyword);
            }
          else if (!ustricmp (source->keyword, L"text-versionid"))
            {
              ret.include_version_id = utob (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-indent-preamble"))
            {
              ret.indent_preambles = utob (uadv (source->keyword));
            }
          else if (!ustricmp (source->keyword, L"text-bullet"))
            {
              ret.bullet.text = uadv (source->keyword);
            }
        }
    }

  return ret;
}

void
text_backend (paragraph * sourceform, keywordlist * keywords, indexdata * idx)
{
  paragraph *p;
  textconfig conf;
  word *prefix, *body, *wp;
  word spaceword;
  FILE *fp;
  char *prefixextra;
  int indentb, indenta;

  IGNORE (keywords);            /* we don't happen to need this */
  IGNORE (idx);                 /* or this */

  conf = text_configure (sourceform);

  /*
   * Determine the output file name, and open the output file
   *
   * FIXME: want configurable output file names here. For the
   * moment, we'll just call it `output.txt'.
   */
  fp = fopen ("output.txt", "w");
  if (!fp)
    {
      error (err_cantopenw, "output.txt");
      return;
    }

  /* Do the title */
  for (p = sourceform; p; p = p->next)
    if (p->type == para_Title)
      text_heading (fp, NULL, NULL, p->words,
                    conf.atitle, conf.indent, conf.width);

  /* Do the preamble and copyright */
  for (p = sourceform; p; p = p->next)
    if (p->type == para_Preamble)
      text_para (fp, NULL, NULL, p->words,
                 conf.indent_preambles ? conf.indent : 0, 0,
                 conf.width + (conf.indent_preambles ? 0 : conf.indent));
  for (p = sourceform; p; p = p->next)
    if (p->type == para_Copyright)
      text_para (fp, NULL, NULL, p->words,
                 conf.indent_preambles ? conf.indent : 0, 0,
                 conf.width + (conf.indent_preambles ? 0 : conf.indent));

  /* Do the main document */
  for (p = sourceform; p; p = p->next)
    switch (p->type)
      {

        /*
         * Things we ignore because we've already processed them or
         * aren't going to touch them in this pass.
         */
      case para_IM:
      case para_BR:
      case para_Biblio:        /* only touch BiblioCited */
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
        text_heading (fp, p->kwtext, p->kwtext2, p->words,
                      conf.achapter, conf.indent, conf.width);
        break;

      case para_Heading:
      case para_Subsect:
        text_heading (fp, p->kwtext, p->kwtext2, p->words,
                      conf.asect[p->aux >=
                                 conf.nasect ? conf.nasect - 1 : p->aux],
                      conf.indent, conf.width);
        break;

      case para_Rule:
        text_rule (fp, conf.indent, conf.width);
        break;

      case para_Normal:
      case para_BiblioCited:
      case para_Bullet:
      case para_NumberedList:
        if (p->type == para_Bullet)
          {
            prefix = &conf.bullet;
            prefixextra = NULL;
            indentb = conf.listindentbefore;
            indenta = conf.listindentafter;
          }
        else if (p->type == para_NumberedList)
          {
            prefix = p->kwtext;
            prefixextra = ".";  /* FIXME: configurability */
            indentb = conf.listindentbefore;
            indenta = conf.listindentafter;
          }
        else
          {
            prefix = NULL;
            prefixextra = NULL;
            indentb = indenta = 0;
          }
        if (p->type == para_BiblioCited)
          {
            body = dup_word_list (p->kwtext);
            for (wp = body; wp->next; wp = wp->next);
            wp->next = &spaceword;
            spaceword.next = p->words;
            spaceword.alt = NULL;
            spaceword.type = word_WhiteSpace;
            spaceword.text = NULL;
          }
        else
          {
            wp = NULL;
            body = p->words;
          }
        text_para (fp, prefix, prefixextra, body,
                   conf.indent + indentb, indenta,
                   conf.width - indentb - indenta);
        if (wp)
          {
            wp->next = NULL;
            free_word_list (body);
          }
        break;

      case para_Code:
        text_codepara (fp, p->words, conf.indent + conf.indent_code,
                       conf.width - 2 * conf.indent_code);
        break;
      }

  /* Do the version ID */
  if (conf.include_version_id)
    {
      for (p = sourceform; p; p = p->next)
        if (p->type == para_VersionID)
          text_versionid (fp, p->words);
    }

  /*
   * Tidy up
   */
  fclose (fp);
  {
    int i;
    sfree (conf.achapter.number_suffix);
    for (i = 0; i < conf.nasect; i++)
      sfree (conf.asect[i].number_suffix);
    sfree (conf.asect);
    sfree (conf.bullet.text);
  }
}

/*
 * Convert a wide string into a string of chars. If `result' is
 * non-NULL, mallocs the resulting string and stores a pointer to
 * it in `*result'. If `result' is NULL, merely checks whether all
 * characters in the string are feasible for the output character
 * set.
 *
 * Return is nonzero if all characters are OK. If not all
 * characters are OK but `result' is non-NULL, a result _will_
 * still be generated!
 */
static int
text_convert (wchar_t * s, char **result)
{
  /*
   * FIXME. Currently this is ISO8859-1 only.
   */
  int doing = (result != 0);
  int ok = TRUE;
  char *p = NULL;
  int plen = 0, psize = 0;

  for (; *s; s++)
    {
      wchar_t c = *s;
      char outc;

      if ((c >= 32 && c <= 126) || (c >= 160 && c <= 255))
        {
          /* Char is OK. */
          outc = (char) c;
        }
      else
        {
          /* Char is not OK. */
          ok = FALSE;
          outc = 0xBF;          /* approximate the good old DEC `uh?' */
        }
      if (doing)
        {
          if (plen >= psize)
            {
              psize = plen + 256;
              p = resize (p, psize);
            }
          p[plen++] = outc;
        }
    }
  if (doing)
    {
      p = resize (p, plen + 1);
      p[plen] = '\0';
      *result = p;
    }
  return ok;
}

static void
text_rdaddwc (rdstringc * rs, word * text, word * end)
{
  char *c;

  for (; text && text != end; text = text->next)
    switch (text->type)
      {
      case word_HyperLink:
      case word_HyperEnd:
      case word_UpperXref:
      case word_LowerXref:
      case word_XrefEnd:
      case word_IndexRef:
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
        assert (text->type != word_CodeQuote &&
                text->type != word_WkCodeQuote);
        if (towordstyle (text->type) == word_Emph &&
            (attraux (text->aux) == attr_First ||
             attraux (text->aux) == attr_Only))
          rdaddc (rs, '_');     /* FIXME: configurability */
        else if (towordstyle (text->type) == word_Code &&
                 (attraux (text->aux) == attr_First ||
                  attraux (text->aux) == attr_Only))
          rdaddc (rs, '`');     /* FIXME: configurability */
        if (removeattr (text->type) == word_Normal)
          {
            if (text_convert (text->text, &c))
              rdaddsc (rs, c);
            else
              text_rdaddwc (rs, text->alt, NULL);
            sfree (c);
          }
        else if (removeattr (text->type) == word_WhiteSpace)
          {
            rdaddc (rs, ' ');
          }
        else if (removeattr (text->type) == word_Quote)
          {
            rdaddc (rs, quoteaux (text->aux) == quote_Open ? '`' : '\'');
            /* FIXME: configurability */
          }
        if (towordstyle (text->type) == word_Emph &&
            (attraux (text->aux) == attr_Last ||
             attraux (text->aux) == attr_Only))
          rdaddc (rs, '_');     /* FIXME: configurability */
        else if (towordstyle (text->type) == word_Code &&
                 (attraux (text->aux) == attr_Last ||
                  attraux (text->aux) == attr_Only))
          rdaddc (rs, '\'');    /* FIXME: configurability */
        break;
      }
}

static int text_width (word *);

static int
text_width_list (word * text)
{
  int w = 0;
  while (text)
    {
      w += text_width (text);
      text = text->next;
    }
  return w;
}

static int
text_width (word * text)
{
  switch (text->type)
    {
    case word_HyperLink:
    case word_HyperEnd:
    case word_UpperXref:
    case word_LowerXref:
    case word_XrefEnd:
    case word_IndexRef:
      return 0;

    case word_Normal:
    case word_Emph:
    case word_Code:
    case word_WeakCode:
      return (((text->type == word_Emph ||
                text->type == word_Code)
               ? (attraux (text->aux) == attr_Only ? 2 :
                  attraux (text->aux) == attr_Always ? 0 : 1)
               : 0) +
              (text_convert (text->text, NULL) ?
               ustrlen (text->text) : text_width_list (text->alt)));

    case word_WhiteSpace:
    case word_EmphSpace:
    case word_CodeSpace:
    case word_WkCodeSpace:
    case word_Quote:
    case word_EmphQuote:
    case word_CodeQuote:
    case word_WkCodeQuote:
      assert (text->type != word_CodeQuote && text->type != word_WkCodeQuote);
      return (((towordstyle (text->type) == word_Emph ||
                towordstyle (text->type) == word_Code)
               ? (attraux (text->aux) == attr_Only ? 2 :
                  attraux (text->aux) == attr_Always ? 0 : 1) : 0) + 1);
    }
  return 0;                     /* should never happen */
}

static void
text_heading (FILE * fp, word * tprefix, word * nprefix, word * text,
              alignstruct align, int indent, int width)
{
  rdstringc t = { 0, 0, NULL };
  int margin, length;
  int firstlinewidth, wrapwidth;
  wrappedline *wrapping, *p;

  if (align.just_numbers && nprefix)
    {
      char *c;
      text_rdaddwc (&t, nprefix, NULL);
      if (text_convert (align.number_suffix, &c))
        {
          rdaddsc (&t, c);
          sfree (c);
        }
    }
  else if (!align.just_numbers && tprefix)
    {
      char *c;
      text_rdaddwc (&t, tprefix, NULL);
      if (text_convert (align.number_suffix, &c))
        {
          rdaddsc (&t, c);
          sfree (c);
        }
    }
  margin = length = (t.text ? strlen (t.text) : 0);

  if (align.align == LEFTPLUS)
    {
      margin = indent - margin;
      if (margin < 0)
        margin = 0;
      firstlinewidth = indent + width - margin - length;
      wrapwidth = width;
    }
  else if (align.align == LEFT || align.align == CENTRE)
    {
      margin = 0;
      firstlinewidth = indent + width - length;
      wrapwidth = indent + width;
    }

  wrapping = wrap_para (text, firstlinewidth, wrapwidth, text_width);
  for (p = wrapping; p; p = p->next)
    {
      text_rdaddwc (&t, p->begin, p->end);
      length = (t.text ? strlen (t.text) : 0);
      if (align.align == CENTRE)
        {
          margin = (indent + width - length) / 2;
          if (margin < 0)
            margin = 0;
        }
      fprintf (fp, "%*s%s\n", margin, "", t.text);
      if (align.underline != L'\0')
        {
          char *u, uc;
          wchar_t uw[2];
          uw[0] = align.underline;
          uw[1] = L'\0';
          text_convert (uw, &u);
          uc = u[0];
          sfree (u);
          fprintf (fp, "%*s", margin, "");
          while (length--)
            putc (uc, fp);
          putc ('\n', fp);
        }
      if (align.align == LEFTPLUS)
        margin = indent;
      else
        margin = 0;
      sfree (t.text);
      t = empty_rdstringc;
    }
  wrap_free (wrapping);
  putc ('\n', fp);

  sfree (t.text);
}

static void
text_rule (FILE * fp, int indent, int width)
{
  while (indent--)
    putc (' ', fp);
  while (width--)
    putc ('-', fp);             /* FIXME: configurability! */
  putc ('\n', fp);
  putc ('\n', fp);
}

static void
text_para (FILE * fp, word * prefix, char *prefixextra, word * text,
           int indent, int extraindent, int width)
{
  wrappedline *wrapping, *p;
  rdstringc pfx = { 0, 0, NULL };
  int e;
  int firstlinewidth = width;

  if (prefix)
    {
      text_rdaddwc (&pfx, prefix, NULL);
      if (prefixextra)
        rdaddsc (&pfx, prefixextra);
      fprintf (fp, "%*s%s", indent, "", pfx.text);
      /* If the prefix is too long, shorten the first line to fit. */
      e = extraindent - strlen (pfx.text);
      if (e < 0)
        {
          firstlinewidth += e;  /* this decreases it, since e < 0 */
          if (firstlinewidth < 0)
            {
              e = indent + extraindent;
              firstlinewidth = width;
              fprintf (fp, "\n");
            }
          else
            e = 0;
        }
      sfree (pfx.text);
    }
  else
    e = indent + extraindent;

  wrapping = wrap_para (text, firstlinewidth, width, text_width);
  for (p = wrapping; p; p = p->next)
    {
      rdstringc t = { 0, 0, NULL };
      text_rdaddwc (&t, p->begin, p->end);
      fprintf (fp, "%*s%s\n", e, "", t.text);
      e = indent + extraindent;
      sfree (t.text);
    }
  wrap_free (wrapping);
  putc ('\n', fp);
}

static void
text_codepara (FILE * fp, word * text, int indent, int width)
{
  for (; text; text = text->next)
    if (text->type == word_WeakCode)
      {
        char *c;
        text_convert (text->text, &c);
        if (strlen (c) > (size_t) width)
          {
            /* FIXME: warn */
          }
        fprintf (fp, "%*s%s\n", indent, "", c);
        sfree (c);
      }

  putc ('\n', fp);
}

static void
text_versionid (FILE * fp, word * text)
{
  rdstringc t = { 0, 0, NULL };

  rdaddc (&t, '[');             /* FIXME: configurability */
  text_rdaddwc (&t, text, NULL);
  rdaddc (&t, ']');             /* FIXME: configurability */

  fprintf (fp, "%s\n", t.text);
  sfree (t.text);
}
