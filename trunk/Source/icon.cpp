#include "Platform.h"
#include "util.h"
#include "lang.h"

#include <stdio.h>
#include <stdexcept>

using namespace std;

extern int g_display_errors;
extern FILE *g_output;

// Icon editing structures
typedef struct {
  WORD wReserved;
  WORD wIsIcon;
  WORD wCount;
} IconGroupHeader;

typedef struct {
  BYTE bWidth;
  BYTE bHeight;
  BYTE bPaletteEntries;
  BYTE bReserved;
  WORD wPlanes;
  WORD wBitsPerPixel;
  DWORD dwRawSize;
  DWORD dwImageOffset;
} FileIconGroupEntry;

typedef struct {
  BYTE bWidth;
  BYTE bHeight;
  BYTE bPaletteEntries;
  BYTE bReserved;
  WORD wPlanes;
  WORD wBitsPerPixel;
  DWORD dwRawSize;
  WORD wRsrcId;
} RsrcIconGroupEntry;

#define SIZEOF_RSRC_ICON_GROUP_ENTRY 14

static FILE * open_icon(const char* filename, IconGroupHeader *igh)
{
  FILE* f = FOPEN(filename, "rb");
  if (!f)
    throw runtime_error("can't open file");

  if (!fread(igh, sizeof(IconGroupHeader), 1, f))
    throw runtime_error("unable to read file");

  FIX_ENDIAN_INT16_INPLACE(igh->wIsIcon);
  FIX_ENDIAN_INT16_INPLACE(igh->wReserved);
  FIX_ENDIAN_INT16_INPLACE(igh->wCount);

  if (igh->wIsIcon != 1 || igh->wReserved != 0)
    throw runtime_error("invalid icon file");

  return f;
}

// replace_icon, must get an initialized resource editor
void replace_icon(CResourceEditor* re, WORD wIconId, const char* filename)
{
  IconGroupHeader igh, *new_igh;
  FILE *f = open_icon(filename, &igh);

  BYTE* rsrcIconGroup = (BYTE*)malloc(sizeof(IconGroupHeader) + igh.wCount*SIZEOF_RSRC_ICON_GROUP_ENTRY);
  if (!rsrcIconGroup) throw bad_alloc();

  CopyMemory(rsrcIconGroup, &igh, sizeof(IconGroupHeader));

  new_igh = (IconGroupHeader *) rsrcIconGroup;
  FIX_ENDIAN_INT16_INPLACE(new_igh->wIsIcon);
  FIX_ENDIAN_INT16_INPLACE(new_igh->wReserved);
  FIX_ENDIAN_INT16_INPLACE(new_igh->wCount);

  RsrcIconGroupEntry* ige = (RsrcIconGroupEntry*)(rsrcIconGroup + sizeof(IconGroupHeader));

  int i = 1;

  // Delete old icons
  while (re->UpdateResourceA(RT_ICON, MAKEINTRESOURCE(i++), NSIS_DEFAULT_LANG, 0, 0));

  for (i = 0; i < igh.wCount; i++) {
    fread(ige, sizeof(FileIconGroupEntry)-sizeof(DWORD), 1, f);

    DWORD dwRawSize = FIX_ENDIAN_INT32(ige->dwRawSize);

    ige->wRsrcId = FIX_ENDIAN_INT16(i + 1);

    DWORD dwOffset;
    fread(&dwOffset, sizeof(DWORD), 1, f);

    FIX_ENDIAN_INT32_INPLACE(dwOffset);

    fpos_t pos;
    fgetpos(f, &pos);

    if (fseek(f, dwOffset, SEEK_SET)) {
      free(rsrcIconGroup);
      throw runtime_error("corrupted icon file, too small");
    }
    BYTE* iconData = (BYTE*)malloc(dwRawSize);
    if (!iconData) {
      free(rsrcIconGroup);
      throw bad_alloc();
    }
    fread(iconData, sizeof(BYTE), dwRawSize, f);
    re->UpdateResourceA(RT_ICON, MAKEINTRESOURCE(i+1), NSIS_DEFAULT_LANG, iconData, dwRawSize);
    free(iconData);

    fsetpos(f, &pos);

    // Seems like the compiler refuses to increase the pointer by just 14.
    // If you'll replace this line by ige++ you will get unwanted results.
    ige = (RsrcIconGroupEntry*)((BYTE*)ige + SIZEOF_RSRC_ICON_GROUP_ENTRY);
  }

  fclose(f);

  re->UpdateResourceA(RT_GROUP_ICON, MAKEINTRESOURCE(wIconId), NSIS_DEFAULT_LANG, rsrcIconGroup, sizeof(IconGroupHeader) + igh.wCount*SIZEOF_RSRC_ICON_GROUP_ENTRY);

  free(rsrcIconGroup);
}

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon that should replace the installer icon data
unsigned char* generate_uninstall_icon_data(const char* filename, size_t &size)
{
  int i;

  IconGroupHeader igh;
  FILE *f = open_icon(filename, &igh);

  int iNewIconSize = 0;
  FileIconGroupEntry ige;

  DWORD* offsets = (DWORD*)malloc(sizeof(DWORD)*igh.wCount);
  DWORD* rawSizes = (DWORD*)malloc(sizeof(DWORD)*igh.wCount);
  if (!offsets || !rawSizes) throw bad_alloc();

  for (i = 0; i < igh.wCount; i++) {
    if (!fread(&ige, sizeof(FileIconGroupEntry), 1, f)) throw runtime_error("unable to read file");
    offsets[i] = ige.dwImageOffset;
    rawSizes[i] = ige.dwRawSize;
    iNewIconSize += FIX_ENDIAN_INT32(ige.dwRawSize);
  }

  // Before each icon come two DWORDs, one for size and the other for offset (set later)
  // The last size is 0, no offset
  iNewIconSize += sizeof(DWORD)*(1 + igh.wCount*2);

  BYTE* pbUninstIcon = (BYTE*)malloc(iNewIconSize);
  if (!pbUninstIcon) throw bad_alloc();

  BYTE* seeker = pbUninstIcon;

  for (i = 0; i < igh.wCount; i++) {
    *(DWORD*)seeker = rawSizes[i];
    seeker += sizeof(DWORD);
    *(DWORD*)seeker = 0;
    seeker += sizeof(DWORD);
    fseek(f, FIX_ENDIAN_INT32(offsets[i]), SEEK_SET);
    fread(seeker, 1, FIX_ENDIAN_INT32(rawSizes[i]), f);
    seeker += FIX_ENDIAN_INT32(rawSizes[i]);
  }

  // This is how we know there are no more icons (size = 0)
  *(DWORD*)seeker = 0;

  free(offsets);
  free(rawSizes);

  size = iNewIconSize;

  return pbUninstIcon;
}

// Added by Amir Szekely 11th July 2002
#define MY_ASSERT(x, y) if (x) {if (g_display_errors) fprintf(g_output,"\nError finding icon resources: %s -- failing!\n", y);return 0;}

int find_in_dir(PRESOURCE_DIRECTORY rd, WORD id) {
  WORD i = FIX_ENDIAN_INT16(rd->Header.NumberOfNamedEntries);
  WORD l = i + FIX_ENDIAN_INT16(rd->Header.NumberOfIdEntries);

  for (; i < l; i++) {
    if (FIX_ENDIAN_INT16(rd->Entries[i].UName.Id) == id) {
      return i;
    }
  }

  return -1;
}

// Fill the array of icons for uninstall with their offsets
// Returns zero on failure
int generate_unicons_offsets(unsigned char* exeHeader, size_t exeHeaderSize, unsigned char* uninstIconData) {
  DWORD dwResourceSectionVA;

  PIMAGE_NT_HEADERS ntHeaders = CResourceEditor::GetNTHeaders(exeHeader);
  PRESOURCE_DIRECTORY rdRoot = CResourceEditor::GetResourceDirectory(exeHeader, exeHeaderSize, ntHeaders, &dwResourceSectionVA);

  int idx = find_in_dir(rdRoot, (WORD) (long) RT_ICON);
  MY_ASSERT(idx < 0, "no icons found");
  MY_IMAGE_RESOURCE_DIRECTORY_ENTRY rdEntry = rdRoot->Entries[idx];
  FIX_ENDIAN_INT32_INPLACE(rdEntry.UOffset.OffsetToData);
  MY_ASSERT(!rdEntry.UOffset.DirectoryOffset.DataIsDirectory, "bad resource directory");

  PRESOURCE_DIRECTORY rdIcons = PRESOURCE_DIRECTORY(rdEntry.UOffset.DirectoryOffset.OffsetToDirectory + DWORD(rdRoot));

  MY_ASSERT((size_t)rdIcons - (size_t)exeHeader > exeHeaderSize, "corrupted EXE - invalid pointer");

  WORD wNumberOfEntries = FIX_ENDIAN_INT16(rdIcons->Header.NumberOfIdEntries);

  MY_ASSERT(wNumberOfEntries == 0, "no icons found");

  for (WORD i = 0; i < wNumberOfEntries; i++) { // Icons dir can't have named entries
    MY_IMAGE_RESOURCE_DIRECTORY_ENTRY icoEntry = rdIcons->Entries[i];
    FIX_ENDIAN_INT32_INPLACE(icoEntry.UOffset.OffsetToData);

    MY_ASSERT(!icoEntry.UOffset.DirectoryOffset.DataIsDirectory, "bad resource directory");
    PRESOURCE_DIRECTORY rd = PRESOURCE_DIRECTORY(icoEntry.UOffset.DirectoryOffset.OffsetToDirectory + DWORD(rdRoot));
    
    MY_ASSERT((size_t)rd - (size_t)exeHeader > exeHeaderSize, "corrupted EXE - invalid pointer");

    MY_IMAGE_RESOURCE_DIRECTORY_ENTRY datEntry = rd->Entries[0];
    FIX_ENDIAN_INT32_INPLACE(datEntry.UOffset.OffsetToData);

    MY_ASSERT(datEntry.UOffset.DirectoryOffset.DataIsDirectory, "bad resource directory");
    
    PIMAGE_RESOURCE_DATA_ENTRY rde = PIMAGE_RESOURCE_DATA_ENTRY(datEntry.UOffset.OffsetToData + DWORD(rdRoot));

    MY_ASSERT((size_t)rde - (size_t)exeHeader > exeHeaderSize, "corrupted EXE - invalid pointer");

    // find icon to replace
    LPBYTE seeker = uninstIconData;
    while (*seeker) {
      DWORD dwSize = *(DWORD*)seeker;
      seeker += sizeof(DWORD);
      DWORD dwOffset = *(DWORD*)seeker;
      // if we haven't set the offset yet and the size is the same, it's a match
      if (!dwOffset && dwSize == rde->Size)
        break;

      seeker += FIX_ENDIAN_INT32(dwSize) + sizeof(DWORD);

      // reached the end of the list and no match
      MY_ASSERT(!*seeker, "installer, uninstaller icon size mismatch - see the Icon instruction's documentation for more information");
    }

    // Set offset
    DWORD dwOffset = FIX_ENDIAN_INT32(rde->OffsetToData) + DWORD(rdRoot) - dwResourceSectionVA - DWORD(exeHeader);
    *(LPDWORD) seeker = FIX_ENDIAN_INT32(dwOffset);

    MY_ASSERT(dwOffset > exeHeaderSize || dwOffset < (DWORD)rdRoot - (DWORD)exeHeader, "invalid data offset - icon resource probably compressed");
  }

  LPBYTE seeker = uninstIconData;
  while (*seeker) {
    DWORD dwSize = *(DWORD*)seeker;
    seeker += sizeof(DWORD);
    DWORD dwOffset = *(DWORD*)seeker;
    seeker += sizeof(DWORD);
    // offset isn't set which means we found no match for this one
    MY_ASSERT(!dwOffset, "installer, uninstaller number of icons doesn't match - see the Icon instruction's documentation for more information");
    seeker += FIX_ENDIAN_INT32(dwSize);
  }

  return 1;
}
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT