/*
 * biblio.c: process the bibliography
 */

#include <assert.h>
#include "halibut.h"

static wchar_t *gentext(int num)
{
  wchar_t text[22];
  wchar_t *p = text + sizeof(text);
  *--p = L'\0';
  *--p = L']';
  while (num != 0)
  {
    assert(p > text);
    *--p = L"0123456789"[num % 10];
    num /= 10;
  }
  assert(p > text);
  *--p = L'[';
  return ustrdup(p);
}

static void cite_biblio(keywordlist * kl, wchar_t * key, filepos fpos)
{
  keyword *kw = kw_lookup(kl, key);
  if (!kw)
    error(err_nosuchkw, &fpos, key);
  else
  {
    /*
     * We've found a \k reference. If it's a
     * bibliography entry ...
     */
    if (kw->para->type == para_Biblio)
    {
      /*
       * ... then mark the paragraph as cited.
       */
      kw->para->type = para_BiblioCited;
    }
  }
}

/*
 * Make a pass through the source form, generating citation formats
 * for bibliography entries and also marking which bibliography
 * entries are actually cited (or \nocite-ed).
 */

void gen_citations(paragraph * source, keywordlist * kl)
{
  paragraph *para;
  int bibnum = 0;

  for (para = source; para; para = para->next)
  {
    word *ptr;

    /*
     * \BR and \nocite paragraphs get special processing here.
     */
    if (para->type == para_BR)
    {
      keyword *kw = kw_lookup(kl, para->keyword);
      if (!kw)
      {
        error(err_nosuchkw, &para->fpos, para->keyword);
      } else if (kw->text)
      {
        error(err_multiBR, &para->fpos, para->keyword);
      } else
      {
        kw->text = dup_word_list(para->words);
      }
    } else if (para->type == para_NoCite)
    {
      wchar_t *wp = para->keyword;
      while (*wp)
      {
        cite_biblio(kl, wp, para->fpos);
        wp = uadv(wp);
      }
    }

    /*
     * Scan for keyword references.
     */
    for (ptr = para->words; ptr; ptr = ptr->next)
    {
      if (ptr->type == word_UpperXref || ptr->type == word_LowerXref
          || ptr->type == word_FreeTextXref)
        cite_biblio(kl, ptr->text, ptr->fpos);
    }
  }

  /*
   * We're now almost done; all that remains is to scan through
   * the cited bibliography entries and invent default citation
   * texts for the ones that don't already have explicitly
   * provided \BR text.
   */
  for (para = source; para; para = para->next)
  {
    if (para->type == para_BiblioCited)
    {
      keyword *kw = kw_lookup(kl, para->keyword);
      assert(kw != NULL);
      if (!kw->text)
      {
        word *wd = smalloc(sizeof(word));
        wd->text = gentext(++bibnum);
        wd->type = word_Normal;
        wd->alt = NULL;
        wd->next = NULL;
        kw->text = wd;
      }
      para->kwtext = kw->text;
    }
  }
}
