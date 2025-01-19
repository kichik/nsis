/*
 * uservars.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2003 Ramon
 * Copyright (C) 2003-2025 NSIS Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 * 
 * Doxygen comments by Jim Park -- 07/25/2007
 */

#ifndef ___USERVARS___H_____
#define ___USERVARS___H_____

#include "lang.h"

struct uservarstring {
  int name;
  int index;
  int pos;
  int reference;
};

class UserVarsStringList : public SortedStringListND<struct uservarstring>
{
  public:
    /* Default constructor */
    UserVarsStringList() : m_index(0) {}

    /* Destructor */
    virtual ~UserVarsStringList() {}

    /**
     * Adds a name to the UserVarsStringList.  Sets reference count to
     * ref_count.
     *
     * @param name The User variable string to store.
     * @param ref_count The reference count to store.  Default is 0.
     * @return The index of the added variable string.
     */
    int add(const TCHAR *name, int ref_count = 0 )
    {
      int pos=SortedStringListND<struct uservarstring>::add(name);
      if (pos == -1) return -1;

      uservarstring* ustr = ((uservarstring*) m_gr.get()) + pos;
      ustr->index     = m_index;
      ustr->pos       = pos;
      ustr->reference = ref_count;

      int temp = m_index;
      m_index++;

      return temp;
    }

    /**
     * Get the index of the string that matches 'name.'
     *
     * @param name The name of the string to search for.
     * @param n_chars If -1, match entire string, otherwise compare only
     * n_chars worth of characters.
     * @return The index position of the structure where structure.name ==
     * name.
     */
    int get(const TCHAR *name, int n_chars = -1) const
    {
      int v = SortedStringListND<struct uservarstring>::find(name, n_chars);
      if (v == -1) return -1;
      return (((struct uservarstring*) m_gr.get())[v].index);
    }

    /**
     * Get count of strings.
     *
     * @return The count of strings.
     */
    int getnum() const
    {
       return m_index;
    }

    /**
     * Given the index of the structure, return the reference count.
     *
     * @return The reference count of the nth uservarstring structure.
     * If not found, returns -1.
     */
    int get_reference(int idx) const
    {
      int pos=get_internal_idx(idx);
      if (pos==-1) return -1;
      return (((struct uservarstring*) m_gr.get())[pos].reference);
    }

    /**
     * Given the index of the structure, increment the reference count.
     *
     * @return The previous reference count (before the increment).
     * If not found, returns -1.
     */
    int inc_reference(int idx)
    {
      int pos=get_internal_idx(idx);
      if (pos==-1) return -1;
      return ((struct uservarstring*) m_gr.get())[pos].reference++;
    }

    /**
     * Given the index of the structure, return the string value
     * of the name.
     *
     * @return String value of the name as TCHAR*.
     * If not found, returns NULL.
     */
    TCHAR *idx2name(int idx)
    {
      int pos=get_internal_idx(idx);
      if (pos==-1) return NULL;
      struct uservarstring *data=(struct uservarstring *) m_gr.get();      
      return ((TCHAR*) m_strings.get() + data[pos].name);
    }

  private:
    int m_index;

    int get_internal_idx(int idx) const
    {
      struct uservarstring *data=(struct uservarstring *) m_gr.get();
      for (int i = 0; i < m_index; i++)
      {
        if (data[i].index == idx)
        {
          return i;
        }
      }
      return -1;
    }
};

#endif
