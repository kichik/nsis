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

static FILE * open_icon(const char* filename, IconGroupHeader& igh)
{
  FILE* f = FOPEN(filename, "rb");
  if (!f)
    throw runtime_error("can't open file");

  if (!fread(&igh, sizeof(IconGroupHeader), 1, f))
    throw runtime_error("unable to read header from file");

  FIX_ENDIAN_INT16_INPLACE(igh.wIsIcon);
  FIX_ENDIAN_INT16_INPLACE(igh.wReserved);
  FIX_ENDIAN_INT16_INPLACE(igh.wCount);

  if (igh.wIsIcon != 1 || igh.wReserved != 0)
    throw runtime_error("invalid icon file");

  return f;
}

void free_loaded_icon(IconGroup icon)
{
  for (IconGroup::size_type i = 0; i < icon.size(); i++)
  {
    delete [] icon[i].data;
  }
}

IconGroup load_icon_res(CResourceEditor* re, WORD id)
{
  IconGroupHeader* header;
  IconGroup result;

  LPBYTE group = re->GetResourceA(
    RT_GROUP_ICON, MAKEINTRESOURCE(id), NSIS_DEFAULT_LANG);

  if (!group)
    throw runtime_error("can't find icon group");

  header = (IconGroupHeader*) group;

  for (WORD i = 0; i < FIX_ENDIAN_INT16(header->wCount); i++)
  {
    Icon icon;
    icon.index = i;

    RsrcIconGroupEntry* entry = (RsrcIconGroupEntry*) (group
      + sizeof(IconGroupHeader) + SIZEOF_RSRC_ICON_GROUP_ENTRY * i);

    memcpy(&icon.meta, &entry->header, sizeof(IconGroupEntry));

    WORD rsrc_id = FIX_ENDIAN_INT16(entry->wRsrcId);

    icon.data = re->GetResourceA(RT_ICON, MAKEINTRESOURCE(rsrc_id), NSIS_DEFAULT_LANG);

    if (!icon.data)
    {
      free_loaded_icon(result);
      throw runtime_error("can't find icon");
    }

    result.push_back(icon);
  }

  return result;
}

IconGroup load_icon_file(const char* filename)
{
  IconGroupHeader iconHeader;
  IconGroup result;

  FILE *file = open_icon(filename, iconHeader);

  for (WORD i = 0; i < iconHeader.wCount; i++)
  {
    Icon icon;
    icon.index = i;
    icon.data = NULL;

    if (!fread(&icon.meta, sizeof(IconGroupEntry), 1, file))
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

    if (!fread(&iconOffset, sizeof(DWORD), 1, file))
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

    if (!fread(icon.data, size, 1, file))
    {
      free_loaded_icon(result);
      throw runtime_error("unable to read icon from file");
    }

    fsetpos(file, &pos);

    result.push_back(icon);
  }

  return result;
}

typedef struct
{
  unsigned index1;
  unsigned index2;
  DWORD size;
} IconPair;

bool compare_icon(Icon a, Icon b)
{
  return FIX_ENDIAN_INT32(a.meta.dwRawSize) > FIX_ENDIAN_INT32(b.meta.dwRawSize);
}

static IconGroup sort_icon(IconGroup icon)
{
  IconGroup sorted = icon;
  sort(sorted.begin(), sorted.end(), compare_icon);
  return sorted;
}

static vector<IconPair> get_icon_order(IconGroup icon1, IconGroup icon2)
{
  IconGroup sorted_icons1 = sort_icon(icon1);
  IconGroup sorted_icons2 = sort_icon(icon2);

  IconGroup::size_type shared_count = min(sorted_icons1.size(), sorted_icons2.size());
  IconGroup::size_type total_count = max(sorted_icons1.size(), sorted_icons2.size());

  vector<IconPair> result;
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
    }

    if (i < sorted_icons2.size())
    {
      pair.index2 = sorted_icons2[i].index;
      pair.index1 = 0xffff;
      pair.size = FIX_ENDIAN_INT32(sorted_icons2[i].meta.dwRawSize);
    }

    result.push_back(pair);
  }

  return result;
}

static LPBYTE generate_icon_group(IconGroup icon, vector<IconPair> order, bool first)
{
  LPBYTE group = new BYTE[
    sizeof(IconGroupHeader) // header
    + order.size() * SIZEOF_RSRC_ICON_GROUP_ENTRY // entries
  ];

  IconGroupHeader* header = (IconGroupHeader*) group;

  header->wReserved = 0;
  header->wIsIcon   = FIX_ENDIAN_INT16(1);
  header->wCount    = FIX_ENDIAN_INT16(icon.size());

  for (IconGroup::size_type i = 0; i < icon.size(); i++)
  {
    RsrcIconGroupEntry* entry = (RsrcIconGroupEntry*)
      &group[sizeof(IconGroupHeader) + SIZEOF_RSRC_ICON_GROUP_ENTRY * i];
    unsigned index = first ? order[i].index1 : order[i].index2;

    memcpy(&entry->header, &icon[index].meta, sizeof(IconGroupEntry));
    entry->wRsrcId = FIX_ENDIAN_INT16(i + 1);
  }

  return group;
}

// set_icon, must get an initialized resource editor
void set_icon(CResourceEditor* re, WORD wIconId, IconGroup icon1, IconGroup icon2)
{
  vector<IconPair> order = get_icon_order(icon1, icon2);

  // genreate group
  LPBYTE group1 = generate_icon_group(icon1, order, true);

  // set group
  size_t group_size = sizeof(IconGroupHeader) // header
    + order.size() * SIZEOF_RSRC_ICON_GROUP_ENTRY; // entries

  re->UpdateResourceA(RT_GROUP_ICON, MAKEINTRESOURCE(wIconId), NSIS_DEFAULT_LANG, group1, group_size);

  // delete old icons
  unsigned i = 1;
  while (re->UpdateResourceA(RT_ICON, MAKEINTRESOURCE(i++), NSIS_DEFAULT_LANG, 0, 0));

  // set new icons
  IconGroup::size_type order_index;
  for (order_index = 0; order_index < order.size(); order_index++)
  {
    LPBYTE data = new BYTE[order[order_index].size];
    memset(data, 0, order[order_index].size);

    if (order_index < icon1.size())
    {
      Icon* icon = &icon1[order[order_index].index1];
      memcpy(data, icon->data, FIX_ENDIAN_INT32(icon->meta.dwRawSize));
    }

    re->UpdateResourceA(RT_ICON, MAKEINTRESOURCE(order_index + 1), NSIS_DEFAULT_LANG, data, order[order_index].size);

    delete [] data;
  }
}

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon that should replace the installer icon data
unsigned char* generate_uninstall_icon_data(IconGroup icon1, IconGroup icon2, size_t &data_size)
{
  IconGroup::size_type i;
  vector<IconPair> order = get_icon_order(icon1, icon2);

  // genreate group
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
  *(LPDWORD) seeker = FIX_ENDIAN_INT32(group_size);
  seeker += sizeof(DWORD);
  *(LPDWORD) seeker = 0;
  seeker += sizeof(DWORD);

  memcpy(seeker, group, group_size);
  seeker += group_size;

  // fill entries
  for (i = 0; i < icon2.size(); i++)
  {
    Icon* icon = &icon2[order[i].index2];
    DWORD size = FIX_ENDIAN_INT32(icon->meta.dwRawSize);

    *(LPDWORD) seeker = FIX_ENDIAN_INT32(size);
    seeker += sizeof(DWORD);
    *(LPDWORD) seeker = 0;
    seeker += sizeof(DWORD);

    memcpy(seeker, icon->data, size);
    seeker += size;
  }

  // add terminator
  *(LPDWORD) seeker = 0;

  // done
  return uninst_data;
}

// Fill the array of icons for uninstall with their offsets
// Returns zero on failure
int generate_unicons_offsets(LPBYTE exeHeader, size_t exeHeaderSize, LPBYTE uninstIconData, WORD wIconId) {
  try
  {
    DWORD offset;
    DWORD size;

    CResourceEditor re(exeHeader, exeHeaderSize);

    LPBYTE seeker = uninstIconData;

    offset = re.GetResourceOffsetA(RT_GROUP_ICON, MAKEINTRESOURCE(wIconId), NSIS_DEFAULT_LANG);

    size = *(LPDWORD)seeker;
    seeker += sizeof(DWORD);
    *(LPDWORD) seeker = FIX_ENDIAN_INT32(offset);
    seeker += sizeof(DWORD);

    seeker += FIX_ENDIAN_INT32(size);

    WORD icon_index = 1;

    while (*(LPDWORD)seeker)
    {
      offset = re.GetResourceOffsetA(RT_ICON, MAKEINTRESOURCE(icon_index), NSIS_DEFAULT_LANG);

      if (offset > exeHeaderSize)
      {
        throw runtime_error("invalid icon offset (possibly compressed icon)");
      }

      size = *(LPDWORD)seeker;
      seeker += sizeof(DWORD);
      *(LPDWORD) seeker = FIX_ENDIAN_INT32(offset);
      seeker += sizeof(DWORD);

      seeker += FIX_ENDIAN_INT32(size);

      icon_index++;
    }
  }
  catch (const exception& e)
  {
    if (g_display_errors)
      fprintf(g_output, "\nError generating uninstaller icon: %s -- failing!\n", e.what());
    return 0;
  }

  return 1;
}
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT
