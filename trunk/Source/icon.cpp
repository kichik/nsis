/*
 * icon.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2025 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */


#include "Platform.h"
#include "icon.h"
#include "util.h"
#include "lang.h"

#include <stdio.h>
#include <stdexcept>
#include <vector>
#include <algorithm>

using namespace std;

extern int g_display_errors;
extern FILE *g_output;

#define SIZEOF_RSRC_ICON_GROUP_ENTRY 14
enum { ICO_TYPE_ICON = 1, ICO_TYPE_CURSOR = 2 };

static bool is_valid_header(const void*headerdata)
{
  assert(sizeof(IconGroupHeader) == 6);
  IconGroupHeader*pGH = (IconGroupHeader*) headerdata;
  if (pGH->wReserved != FIX_ENDIAN_INT16(0x0000)) return false;
  WORD type = FIX_ENDIAN_INT16(pGH->wType);
  if (type != ICO_TYPE_ICON && type != ICO_TYPE_CURSOR) return false;
  return FIX_ENDIAN_INT16(pGH->wCount) != 0;
}

template<class S> static WORD read_icon_header(S*f, IconGroupHeader& igh)
{
  if (!freadall(&igh, sizeof(IconGroupHeader), f))
  {
    fclose(f);
    throw runtime_error("unable to read header from file");
  }

  if (!is_valid_header(&igh))
  {
    igh.wType = igh.wCount = 0;
    fclose(f);
    throw runtime_error("invalid icon file");
  }

  FIX_ENDIAN_INT16_INPLACE(igh.wReserved);
  FIX_ENDIAN_INT16_INPLACE(igh.wType);
  FIX_ENDIAN_INT16_INPLACE(igh.wCount);
  return igh.wCount;
}

void free_loaded_icon(IconGroup&icon)
{
  for (IconGroup::size_type i = 0; i < icon.size(); i++)
    delete [] icon[i].data;
  icon.clear();
}

IconGroup load_icon_res(CResourceEditor* re, LPWSTR RT, WORD RN, LANGID RL)
{
  IconGroupHeader* header;
  IconGroup result;

  LPBYTE group = re->GetResource(RT, RN, RL);
  if (!group)
    throw runtime_error("can't find icon group");

  header = (IconGroupHeader*) group;

  // Note: To handle cursors, use CResourceEditor::ExtractIcoCur
  if (MAKEINTRESOURCE((size_t) RT) != RT_GROUP_ICON)
    throw runtime_error("unsupported type");

  for (WORD i = 0; i < FIX_ENDIAN_INT16(header->wCount); i++)
  {
    Icon icon;
    icon.index = i;

    RsrcIconGroupEntry* entry = (RsrcIconGroupEntry*) (group
      + sizeof(IconGroupHeader) + SIZEOF_RSRC_ICON_GROUP_ENTRY * i);

    memcpy(&icon.meta, &entry->header, sizeof(IconGroupEntry));

    WORD rsrc_id = FIX_ENDIAN_INT16(entry->wRsrcId);
    icon.data = re->GetResource(RT_ICON, rsrc_id, RL);
    if (!icon.data)
    {
      free_loaded_icon(result);
      throw runtime_error("can't find icon");
    }

    result.push_back(icon);
  }

  re->FreeResource(group);
  return result;
}

IconGroup load_icon_res(CResourceEditor* re, WORD id)
{
  return load_icon_res(re, RT_GROUP_ICON, id, NSIS_DEFAULT_LANG);
}

template<class S> static IconGroup load_iconimages_from_stream(const IconGroupHeader&iconHeader, S&file)
{
  IconGroup result;

  for (WORD i = 0; i < iconHeader.wCount; i++)
  {
    Icon icon;
    icon.index = i;
    icon.data = NULL;

    if (!freadall(&icon.meta, sizeof(IconGroupEntry), file))
    {
      free_loaded_icon(result);
      throw runtime_error("unable to read entry from file");
    }

    DWORD size = FIX_ENDIAN_INT32(icon.meta.dwRawSize);
    if (size > 1048576) // magic numbers are great
    {
      free_loaded_icon(result);
      throw runtime_error("invalid icon file size");
    }

    DWORD iconOffset;
    if (!freadall(&iconOffset, sizeof(DWORD), file))
    {
      free_loaded_icon(result);
      throw runtime_error("unable to read offset from file");
    }

    FIX_ENDIAN_INT32_INPLACE(iconOffset);

    fpos_t pos;
    fgetpos(file, &pos);
    if (fseek(file, iconOffset, SEEK_SET))
    {
      free_loaded_icon(result);
      throw runtime_error("corrupted icon file, too small");
    }

    icon.data = new BYTE[size];
    if (!freadall(icon.data, size, file)) die:
    {
      free_loaded_icon(result);
      throw runtime_error("unable to read icon from file");
    }

    if (fsetpos(file, &pos))
      goto die;

    result.push_back(icon);
  }

  return result;
}

template<class S> static IconGroup load_icon_from_stream(S*file)
{
  IconGroupHeader iconHeader;
  read_icon_header(file, iconHeader);
  return load_iconimages_from_stream(iconHeader, file);
}

IconGroup load_icon_file(const TCHAR* filename)
{
  FILE* f = FOPEN(filename, ("rb"));
  if (!f)
    throw runtime_error("can't open file");

  MANAGE_WITH(f, fclose);
  return load_icon_from_stream(f);
}

IconGroup load_icon(const TCHAR* filename)
{
  if (CResourceEditor::IsResProtocol(filename)) // Try opening embedded resource
  {
    CResourceEditor::EXTERNAL x;
    if (CResourceEditor::MapExternal(filename, CResourceEditor::TM_ICON, x))
    {
      CStdFileStreamOnMemory stream(x.Data, x.cbData);
      IconGroup icon = load_icon_from_stream(&stream);
      CResourceEditor::FreeExternal(x);
      return icon;
    }
  }
  return load_icon_file(filename);
}

typedef struct
{
  unsigned index1;
  unsigned index2;
  DWORD size;
  unsigned size_index;
} IconPair;


typedef vector<IconPair> IconPairs;

static bool compare_icon(Icon a, Icon b)
{
  return FIX_ENDIAN_INT32(a.meta.dwRawSize) > FIX_ENDIAN_INT32(b.meta.dwRawSize);
}

static IconGroup sort_icon(IconGroup icon)
{
  IconGroup sorted = icon;
  sort(sorted.begin(), sorted.end(), compare_icon);
  return sorted;
}

static bool compare_pairs_index1(IconPair a, IconPair b)
{
  return a.index1 < b.index1;
}

static bool compare_pairs_index2(IconPair a, IconPair b)
{
  return a.index2 < b.index2;
}

static IconPairs sort_pairs(IconPairs pairs, bool first)
{
  IconPairs sorted = pairs;
  sort(sorted.begin(), sorted.end(), first ? compare_pairs_index1 : compare_pairs_index2);
  return sorted;
}

static IconPairs get_icon_order(IconGroup icon1, IconGroup icon2)
{
  IconGroup sorted_icons1 = sort_icon(icon1);
  IconGroup sorted_icons2 = sort_icon(icon2);

  IconGroup::size_type shared_count = min(sorted_icons1.size(), sorted_icons2.size());
  IconGroup::size_type total_count = max(sorted_icons1.size(), sorted_icons2.size());

  IconPairs result;
  IconGroup::size_type i;

  for (i = 0; i < shared_count; i++)
  {
    IconPair pair;

    pair.index1 = sorted_icons1[i].index;
    pair.index2 = sorted_icons2[i].index;
    pair.size = max(
      FIX_ENDIAN_INT32(sorted_icons1[i].meta.dwRawSize),
      FIX_ENDIAN_INT32(sorted_icons2[i].meta.dwRawSize)
    );
    pair.size_index = truncate_cast(unsigned int,i);

    result.push_back(pair);
  }

  for (; i < total_count; i++)
  {
    IconPair pair;

    if (i < sorted_icons1.size())
    {
      pair.index1 = sorted_icons1[i].index;
      pair.index2 = 0xffff;
      pair.size = FIX_ENDIAN_INT32(sorted_icons1[i].meta.dwRawSize);
      pair.size_index = truncate_cast(unsigned int,i);
    }

    if (i < sorted_icons2.size())
    {
      pair.index2 = sorted_icons2[i].index;
      pair.index1 = 0xffff;
      pair.size = FIX_ENDIAN_INT32(sorted_icons2[i].meta.dwRawSize);
      pair.size_index = truncate_cast(unsigned int,i);
    }

    result.push_back(pair);
  }

  return result;
}

#define destroy_icon_group(p) ( delete [] (p) )
static LPBYTE generate_icon_group(IconGroup icon, IconPairs order, bool first)
{
  size_t groupsize = 
    sizeof(IconGroupHeader) // header
    + order.size() * SIZEOF_RSRC_ICON_GROUP_ENTRY; // entries
  LPBYTE group = new BYTE[groupsize];
  memset(group, 0, groupsize); // Reproducible builds (bug #1230)

  IconGroupHeader* header = (IconGroupHeader*) group;

  header->wReserved = 0;
  header->wType     = FIX_ENDIAN_INT16(ICO_TYPE_ICON);
  header->wCount    = FIX_ENDIAN_INT16((WORD)icon.size());

  order = sort_pairs(order, first);

  for (IconGroup::size_type i = 0; i < icon.size(); i++)
  {
    RsrcIconGroupEntry* entry = (RsrcIconGroupEntry*)
      &group[sizeof(IconGroupHeader) + SIZEOF_RSRC_ICON_GROUP_ENTRY * i];
    unsigned index = first ? order[i].index1 : order[i].index2;

    memcpy(&entry->header, &icon[index].meta, sizeof(IconGroupEntry));
    entry->wRsrcId = FIX_ENDIAN_INT16(order[i].size_index + 1);
  }

  return group;
}

// set_icon, must get an initialized resource editor
void set_main_icon(CResourceEditor* re, WORD wIconId, IconGroup icon1, IconGroup icon2)
{
  IconPairs order = get_icon_order(icon1, icon2);

  // generate group
  LPBYTE group1 = generate_icon_group(icon1, order, true);

  // set group
  size_t group_size = sizeof(IconGroupHeader) // header
    + order.size() * SIZEOF_RSRC_ICON_GROUP_ENTRY; // entries

  re->UpdateResource(RT_GROUP_ICON, wIconId, NSIS_DEFAULT_LANG, group1, (DWORD)group_size, CResourceEditor::TM_RAW); // Update the group entries but not the images
  destroy_icon_group(group1);

  // delete old icons
  unsigned i = 1;
  while (i <= MAIN_ICON_LAST_IMAGE && re->UpdateResource(RT_ICON, i++, NSIS_DEFAULT_LANG, 0, 0));

  // set new icons
  IconGroup::size_type order_index;
  for (order_index = 0; order_index < order.size(); order_index++)
  {
    WORD size_index = order[order_index].size_index;
    DWORD size = order[order_index].size;
    LPBYTE data = new BYTE[size];
    memset(data, 0, size);

    if (order_index < icon1.size())
    {
      Icon* icon = &icon1[order[order_index].index1];
      memcpy(data, icon->data, FIX_ENDIAN_INT32(icon->meta.dwRawSize));
    }

    re->UpdateResource(RT_ICON, size_index + 1, NSIS_DEFAULT_LANG, data, size);

    delete [] data;
  }
}

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon that should replace the installer icon data
unsigned char* generate_uninstall_icon_data(IconGroup icon1, IconGroup icon2, size_t &data_size)
{
  IconGroup::size_type i;
  IconPairs order = get_icon_order(icon1, icon2);

  // generate group
  LPBYTE group = generate_icon_group(icon2, order, false);

  // calculate size
  size_t group_size = sizeof(IconGroupHeader) // header
    + order.size() * SIZEOF_RSRC_ICON_GROUP_ENTRY; // entries

  data_size = group_size // group header
    + sizeof(DWORD) * 2 // offset and size of group header
    + (sizeof(DWORD) * 2) * icon2.size() // offset and size per entry
    + sizeof(DWORD); // terminator

  for (i = 0; i < icon2.size(); i++)
  {
    // add icon sizes
    data_size += FIX_ENDIAN_INT32(icon2[i].meta.dwRawSize);
  }

  // allocate memory
  LPBYTE uninst_data = new BYTE[data_size];
  LPBYTE seeker = uninst_data;

  // fill group header
  *(LPDWORD) seeker = FIX_ENDIAN_INT32((UINT32)group_size);
  seeker += sizeof(DWORD);
  *(LPDWORD) seeker = 0;
  seeker += sizeof(DWORD);

  memcpy(seeker, group, group_size);
  seeker += group_size;

  // fill entries
  for (i = 0; i < icon2.size(); i++)
  {
    Icon* icon = &icon2[order[i].index2];
    DWORD size = icon->meta.dwRawSize;

    memcpy(seeker, &size, sizeof(size));
    seeker += sizeof(size);
    memset(seeker, 0, sizeof(DWORD));
    seeker += sizeof(DWORD);
    memcpy(seeker, icon->data, size);
    seeker += FIX_ENDIAN_INT32(size);
  }

  // add terminator
  memset(seeker, 0, sizeof(DWORD));

  // done
  destroy_icon_group(group);
  return uninst_data;
}

// Fill the array of icons for uninstall with their offsets
// Returns zero on failure
int generate_unicons_offsets(LPBYTE exeHeader, size_t exeHeaderSize, LPBYTE uninstIconData, WORD wIconId)
{
  try
  {
    DWORD offset;
    DWORD size;

    CResourceEditor re(exeHeader, (DWORD)exeHeaderSize, false);

    LPBYTE seeker = uninstIconData;

    offset = re.GetResourceOffset(RT_GROUP_ICON, wIconId, NSIS_DEFAULT_LANG);

    size = FIX_ENDIAN_INT32(*(LPDWORD)seeker);
    seeker += sizeof(DWORD);
    *(LPDWORD) seeker = FIX_ENDIAN_INT32(offset);
    seeker += sizeof(DWORD);

    seeker += size;

    WORD icon_index = 1;

    while (*(LPDWORD)seeker)
    {
      offset = re.GetResourceOffset(RT_ICON, icon_index, NSIS_DEFAULT_LANG);

      if (offset > exeHeaderSize)
      {
        throw runtime_error("invalid icon offset (possibly compressed icon)");
      }

      DWORD real_size = re.GetResourceSize(RT_ICON, icon_index, NSIS_DEFAULT_LANG);

      size = FIX_ENDIAN_INT32(*(LPDWORD)seeker);
      seeker += sizeof(DWORD);

      if (real_size < size) // uninst icon could be smaller, in case we don't have perfect matches
      {
        throw runtime_error("invalid icon size (possibly compressed icon)");
      }

      *(LPDWORD) seeker = FIX_ENDIAN_INT32(offset);
      seeker += sizeof(DWORD);

      seeker += size;

      icon_index++;
    }
  }
  catch (const exception& e)
  {
    if (g_display_errors)
      PrintColorFmtMsg_ERR(_T("\nError generating uninstaller icon: %") NPRIs _T(" -- failing!\n"), CtoTStrParam(e.what()));
    return 0;
  }

  return 1;
}
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT
