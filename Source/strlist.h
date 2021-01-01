/*
 * strlist.h: Implementation of the StringList class.
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support and Doxygen comments by Jim Park -- 07/27/2007
 */

#ifndef _STRLIST_H_
#define _STRLIST_H_

#include "Platform.h"
#include <cstdio>
#include <fstream>
#include "tchar.h"
#include "growbuf.h"
#include "util.h"

/**
 * Implements a list of strings mapped into a straight buffer.  It is
 * virtually always O(n) to do any basic operations on this thing.  Very
 * memory efficient but very slow performance.
 */
class StringList
{
private: // don't copy instances
  StringList(const StringList&);
  void operator=(const StringList&);

public:
  StringList() {}
  ~StringList() {}

  /**
   * Adds a string to the StringList.  If the string already exists in the
   * list, then it just returns the index of the found string.
   *
   * @param str String to add.
   *
   * @param case_sensitive If 0, search for whole string case insensitively.
   * If 1, then search for whole string case sensitively.  If 2, then search
   * for just the end of the string match case sensitively.  Basically it's
   * like searching for regexp('str$') in the list of string.  If -1, then it
   * adds the string regardless of whether there is a match or not!
   *
   * @return the index to the string in TCHARs.
   */
  int add(const TCHAR *str, int case_sensitive);

  /**
   * Search the StringList for a string.  If the string already exists in the
   * list then it returns the index of the found string.  Returns -1 on
   * failure.
   * 
   * @param str String to search for.
   *
   * @param case_sensitive If 0, search for whole string case insensitively.If
   * 1, then search for whole string case <b>sensitively</b>.  If 2, then
   * search for just the <b>end of the string match</b>, case sensitively.
   * Basically it's like searching for regexp('str$') in the list of string.
   *
   * @param idx If not NULL, the *idx is set to the cardinal number of the
   * string.  In other words, it tells you which nth string it is in the
   * list.
   *
   * @return the TCHAR index to the string (not necessarily the byte positional
   * offset).  -1 if not found.
   */
  int find(const TCHAR *str, int case_sensitive, int *idx=NULL) const; // returns -1 if not found

  /**
   * Delete the string at the positional index.
   *
   * @param pos The number of TCHARS to count from the beginning of the buffer
   * before the start of the string.
   */
  void delbypos(int pos);
  void deleteall() { m_gr.resize(0); }

  /**
   * Converts the string index to the positional TCHAR index.  For example,
   * it gives the answer to which TCHAR position is the beginning of the
   * nth string in the list?
   *
   * @param idx The string index.
   */
  int idx2pos(int idx) const;

  /**
   * Get the count of the number of strings in the list.
   * @return the number of string in the list.
   */
  int getnum() const;

  /**
   * Get the buffer straight as a const TCHAR pointer.  Very unwise to use.
   * @return m_gr.m_s cast as a TCHAR*.
   */
  const TCHAR *get() const { return (const TCHAR*) m_gr.get(); }

  /**
   * Get the buffer size (number of TCHARs).
   * @return The buffer size (number of TCHARs).
   */
  int getcount() const { return m_gr.getlen() / sizeof(TCHAR); }

protected:
  GrowBuf m_gr;
};


#include "tstring.h"
/**
 * Stores a list of strings as UTF16LE or raw bytes (multibyte char*).
 */
class ExeHeadStringList
{
private: // don't copy instances
  ExeHeadStringList(const ExeHeadStringList&);
  void operator=(const ExeHeadStringList&);

public:
  ExeHeadStringList()
  {
    m_wide = false;
    m_gr.set_zeroing(true);
  }

  void setunicode(bool unicode) { m_wide = unicode; }
  bool addemptystring() { return true; } // Added by add() when the first real string is added
  int add(const TCHAR *str, WORD codepage, bool processed);
  unsigned int find(const TCHAR *str, WORD codepage, bool processed, char**ppBufMB) const;
  bool get(unsigned int offset, tstring&str) const;
  unsigned int getnum() const;
  unsigned int gettotalsize() const { return m_gr.get() ? m_gr.getlen() : (m_wide ? 2 : 1); }
  const void* getstorageptr() const { return m_gr.get() ? m_gr.get() : (void*)L""; }

protected:
  unsigned int find(const void *str, unsigned int cchF, WORD codepage, bool processed, char**ppBufMB) const;

  GrowBuf m_gr;
  bool m_wide; // Are we storing wide or narrow strings
  enum {WIDEDIV=2}; // ExeHead expects us to provide offsets this way, also helps UTF16 offsets for shell constants to fit in < 0xFF
};


/**
 * This class maintains a list of T types in a GrowBuf sorted by T.name which
 * is assumed to be a string (TCHAR*).  So it's really sort of a 
 * map<TCHAR*, X> where X is whatever else is defined in T.  But T must define
 * a TCHAR* name.
 */
template <class T>
class SortedStringList
{
  public:
    /**
     * Jim Park: Note that SortedStringList actually <b>owns</b> T.name.
     * Yes, this violates all kinds of encapsulation ideas.
     */
    virtual ~SortedStringList()
    {
      T *s=(T*) m_gr.get();
      size_t num = m_gr.getlen()/sizeof(T);

      for (size_t i=0; i<num; i++) {
        free(s[i].name);
      }
    }

    /**
     * This function adds a new T struct with a copy of TCHAR *name into
     * T.name.  But adds it into a sorted position.  All calls to
     * add must be done with the same value for case_sensitive or you
     * can get random behavior.
     *
     * @param name The name which is the "key" to finding the instance of T.
     * @param case_sensitive 1 means case sensitive, 0 insensitive.
     * @return Returns -1 when name already exists and pos if added.
     */
    int add(const TCHAR *name, int case_sensitive=0)
    {
      T newstruct={0,};
      int pos=find(name,case_sensitive,1);
      if (pos==-1) return -1;
      const unsigned long cbName=(unsigned long) (_tcslen(name)+1)*sizeof(TCHAR);
      newstruct.name=(TCHAR*)malloc(cbName);
      if (!newstruct.name)
      {
        extern int g_display_errors;
        extern void quit();
        if (g_display_errors)
        {
          PrintColorFmtMsg_ERR(_T("\nInternal compiler error #12345: SortedStringList malloc(%lu) failed.\n"),(unsigned long)cbName);
        }
        quit();
      }
      _tcscpy(newstruct.name,name);
      m_gr.add(&newstruct,sizeof(T));

      T *s=(T*) m_gr.get();
      memmove(s+pos+1,s+pos, m_gr.getlen()-((pos+1)*sizeof(T)));
      memcpy(s+pos,&newstruct,sizeof(T));

      return pos;
    }

    /**
     * This function does a binary search for the T in the buffer that
     * contains str as its T.name.  It then returns the position of the found
     * T, not in its byte position, but its T position (valid offset to T*).
     *
     * @param str The string to search for in T.name.
     *
     * @param case_sensitive If 1, do a case sensitive search, otherwise, case insensitive.
     *
     * @param returnbestpos If 1, then the function changes behavior.  Instead
     * of looking for the string str in the Ts, it tries to find the best
     * sorted position to insert str into the buffer.
     *
     * @return Returns -1 if not found, position if found.  If returnbestpos=1
     * returns -1 if found, best pos to insert if not found
     */
    int find(const TCHAR *str, int case_sensitive=0, int returnbestpos=0)
    {
      T *data=(T *) m_gr.get();
      int ul= m_gr.getlen()/sizeof(T);
      int ll=0;
      int nextpos=(ul+ll)/2;

      while (ul > ll)
      {
        int res;
        if (case_sensitive)
          res=_tcscmp(str, data[nextpos].name);
        else
          res=_tcsicmp(str, data[nextpos].name);
        if (res==0) return returnbestpos ? -1 : nextpos;
        if (res<0) ul=nextpos;
        else ll=nextpos+1;
        nextpos=(ul+ll)/2;
      }

      return returnbestpos ? nextpos : -1;
    }

    /**
     * This function looks for str in T.name and deletes the T in the
     * buffer.
     *
     * @return Returns 0 on success, 1 on failure.
     */
    int del(const TCHAR *str, int case_sensitive=0)
    {
      int pos=find(str, case_sensitive);
      if (pos==-1) return 1;

      delbypos(pos);

      return 0;
    }

    /**
     * Given a T position, it deletes it from the buffer.  It will
     * move the rest of the Ts to fill its position.
     *
     * @param pos The position of the target for deletion in T* offsets.
     */
    void delbypos(int pos)
    {
      T *db=(T *) m_gr.get();
      free(db[pos].name);
      memmove(db+pos,db+pos+1, m_gr.getlen()-(pos*sizeof(T))-sizeof(T));
      m_gr.resize(m_gr.getlen()-sizeof(T));
    }

  protected:
    TinyGrowBuf m_gr;
};


/**
 * This class maintains a list of T types in a GrowBuf sorted by T.name which
 * is assumed to be an index into m_strings.  So it's really sort of a 
 * map<TCHAR*, X> where X is whatever else is defined in T.  But T must define
 * a int name.
 *
 * This version does not have a delete function, hence the ND designation.
 * Also, because nothing is malloc'ed and free'd, this structure can be
 * placed in a single flat buffer.  (Of course, T itself can be holding
 * pointers to things on the heap, which this structure does not
 * disallow explicitly.)
 */
template <class T>
class SortedStringListND // no delete - can be placed in GrowBuf
{
  public:
    SortedStringListND() { }
    virtual ~SortedStringListND() { }

     /**
      * Adds name into the list of sorted strings.
      *
      * @param name String to store.
      * @param case_sensitive Look for string case sensitively.  Default is 0.
      * @param alwaysreturnpos Always return the position regardless of whether
      * name was inserted into the list or not.  The default is 0.
      *
      * @return Returns -1 when name already exists, otherwise the T offset
      * into which the struct was stored in m_gr.  If alwaysreturnpos
      * is true, then it will return the byte offset regardless of whether
      * the string was found.
      */
    int add(const TCHAR *name, int case_sensitive=0, int alwaysreturnpos=0)
    {
      int where=0;
      T newstruct={0,};

      // Find the best position to insert.
      int pos=find(name,-1,case_sensitive,1,&where);

      if (pos==-1) return alwaysreturnpos ? where : -1;

      // Note that .name is set with the TCHAR* offset into m_strings.
      newstruct.name=m_strings.add(name,((unsigned int)_tcslen(name)+1)*sizeof(TCHAR))/sizeof(TCHAR);

      m_gr.add(&newstruct,sizeof(T));
      T *s=(T*) m_gr.get();
      memmove(s+pos+1, s+pos, m_gr.getlen()-((pos+1)*sizeof(T)));
      memcpy(s+pos, &newstruct, sizeof(T));

      return pos;
    }

   /**
    * This function looks for the string str, in T.name in the buffer m_gr.
    * If it finds it, it returns the position found.  Otherwise, it returns
    * -1.  
    *
    * This behavior changes when returnbestpos == 1.  In this case,
    * it will do the reverse.  It will return -1 when it is found, noting
    * that there is no good place to put this duplicate string.  If it
    * is <b>not</b> found, it returns the position where it ought to be
    * placed.
    *
    * When case_sensitive == -1 and returnbestpos == 1, then when the string
    * is found, it returns
    * the position of the string so that one can overwrite it.  Very strange
    * special case behavior that I'm not sure if anyone actually uses.
    *
    * @param str The key string to search for.
    *
    * @param n_chars The number of characters to compare.  Use -1 to match
    * the entire string.
    *
    * @param case_sensitive 1 = case sensitive, 0 = case insensitive,
    * -1 is a special case where it is case sensitive and overrides the
    * returnbestpos behavior when the string is found.
    *
    * @param returnbestpos If 1, then look for the best position to add the
    * string.  If found in the list, return -1.
    *
    * @param where When str is found, returns the position of the string.
    *
    * @return The position of T where T.name == str.  If returnbestpos != 0
    * then return the best position to add T if not found, otherwise, -1.
    */
    int find
    (
      const TCHAR* str,    /* key to search for */
      int n_chars=-1,      /* if -1, test the entire string, otherwise just n characters */
      int case_sensitive=0,
      int returnbestpos=0, /* if not found, return best pos */
      int*where=0
    ) const
    {
      T *data=(T *) m_gr.get();
      int ul = m_gr.getlen()/sizeof(T);
      int ll = 0;
      int nextpos = (ul+ll)/2;

      // Do binary search on m_gr which is sorted. m_strings is NOT sorted.
      while (ul > ll)
      {
        int res;
        const TCHAR *pCurr = (TCHAR*) m_strings.get() + data[nextpos].name;
        if (n_chars < 0)
        {
          if (case_sensitive)
            res = _tcscmp(str, pCurr);
          else
            res = _tcsicmp(str, pCurr);
        }
        else
        {
          unsigned int pCurr_len = (unsigned int) _tcslen(pCurr);
          if (case_sensitive)
            res = _tcsncmp(str, pCurr, STD_MIN((unsigned int) n_chars, pCurr_len));
          else
            res = _tcsnicmp(str, pCurr, STD_MIN((unsigned int) n_chars, pCurr_len));

          // If there is a match and we are looking for a partial match and
          // n_chars is NOT the length of the current string, then the
          // comparison result is determined by the length comparison.
          if (res == 0 && n_chars != -1 && (unsigned int) n_chars != pCurr_len)
            res = n_chars - pCurr_len;
        }

        // Found!
        if (res==0)
        {
          // Return where we found it in *where.
          if (where) *where = nextpos;

          // If returnbestpos, then we should return -1, otherwise where
          // we found it.  But if (returnbestpos && case_sensitive == -1)
          // returns nextpos.
          return returnbestpos ? (case_sensitive!=-1 ? -1 : nextpos) : nextpos;
        }
        if (res<0) ul=nextpos;
        else ll=nextpos+1;
        nextpos=(ul+ll)/2;
      }

      return returnbestpos ? nextpos : -1;
    }

  protected:
    TinyGrowBuf m_gr;      // Sorted array of Ts
    GrowBuf     m_strings; // Unsorted array of TCHAR strings (contains the .names)
};

/**
 * Structure stored by DefineList.
 */
struct define {
  TCHAR *name;  // key
  TCHAR *value; // value stored
};

/**
 * DefineList is a specialized version of a SortedStringList
 * which is like a string to string mapping class.
 */
class DefineList : public SortedStringList<struct define>
{
  private: // don't copy instances
    DefineList(const DefineList&);
    void operator=(const DefineList&);

  public:
    /* Empty default constructor */
    DefineList() {} // VC6 complains otherwise
    virtual ~DefineList();

    /**
     * Add a name-value pair, case insensitively.
     * 
     * @param name The name of the variable or key to search by.  In a
     * std::map, it would be the .first of the pair.
     * 
     * @param value The value to store.  In a std::map, it would be the.second
     * of the pair.
     * 
     * @return Returns 0 if successful, 1 if already exists.  Errors cause
     * general program exit with error logging.
     */
    int add(const TCHAR *name, const TCHAR *value=_T(""));
    int addn(const TCHAR *name, size_t maxvallen, const TCHAR *value); // maxvallen does not include \0
    int set(const TCHAR *name, const TCHAR *value=_T(""));
    int set_si32(const TCHAR *name, long value);
    int set_ui32(const TCHAR *name, unsigned long value);

    /**
     * This function returns the pointer to the .value TCHAR* that corresponds
     * to the name key.
     *
     * @param name The key to search with.
     * 
     * @return The TCHAR* to the value portion of the define struct.  If not
     * found, returns NULL.
     */
    TCHAR *find(const TCHAR *name);

    /**
     * This function deletes the define struct corresponding to the key 'str'.
     *
     * @return Returns 0 on success, 1 otherwise
     */
    int del(const TCHAR *str);

    /**
     * This function returns the number of define structs in the sorted array.
     */
    int getnum();

    /**
     * Get the .name string of the (num)th define struct in the sorted array.
     *
     * @return Returns 0 if not found, otherwise the pointer to the .name.
     */
    TCHAR *getname(int num);

    /**
     * Get the .value string of the (num)th define struct in the sorted array.
     *
     * @return Returns 0 if not found, otherwise the pointer to the .value.
     */
    TCHAR *getvalue(int num);
};

/**
 * Storage unit for FastStringList.  Contains the byte offset into m_strings.
 */
struct string_t {
  int name;
};

/**
 * This class uses SortedStringListND to implement a "faster" storage of
 * strings.  It sort of implements a std::set<string> that allows you
 * to add to the set and check for existence of the set.
 *
 * It's only faster in the sense that memory moves now only need to occur
 * on the array of string_t structs (or pointers) rather than entire
 * strings.  A truly faster implementation would be using a hash table.
 */
class FastStringList : public SortedStringListND<struct string_t>
{
  private: // don't copy instances
    FastStringList(const FastStringList&);
    void operator=(const FastStringList&);
    
  public:
    /* Empty constructor */
    FastStringList() {} // VC6 complains otherwise

    /* Empty virtual destructor */
    virtual ~FastStringList() {}

    /**
     * Adds name to sorted array and returns the TCHAR* offset of m_strings
     * where it is stored.
     *
     * @param name The string to store.
     *
     * @param case_sensitive Should we store this case sensitively or not?
     * Setting case_sensitive to -1 will cause it to be case sensitive and
     * always overwrite.  (Weird bad behavior).
     *
     * @return The TCHAR* offset of name in m_string as an int.
     */
    int add(const TCHAR *name, int case_sensitive=0);

    /**
     * Get the buffer that contains the list of the strings in the order
     * in which they were added.
     *
     * @return The pointer to m_strings as a TCHAR*.
     */
    TCHAR *get() const;

    /**
     * The size of the collection of m_strings as a count of TCHARs.
     *
     * @return the size of m_strings as count of TCHARs.
     */
    int getcount() const;

    /**
     * The number of strings stored in the sorted array.
     *
     * @return The number of strings stored.
     */
    int getnum() const;
};

#endif//_STRLIST_H_
