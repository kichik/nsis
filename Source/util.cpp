#include <windows.h>
#include <stdio.h>
#include <stdarg.h>
#include <conio.h>
#include "exedata.h"
#include "exehead/fileform.h"
#include "util.h"
#include "strlist.h"

int g_dopause=0;
extern int g_display_errors;
extern FILE *g_output;


void dopause(void)
{
  if (g_dopause)
  {
    if (g_display_errors) fprintf(g_output,"MakeNSIS done - hit enter to close...");
    fflush(stdout);
    int a;
    while ((a=_getch()) != '\r' && a != 27/*esc*/);
  }
}

// Returns 0 if everything is OK
// Returns -1 the bitmap file is invalid or has the wrong size
int update_bitmap(CResourceEditor* re, WORD id, char* filename, int width/*=0*/, int height/*=0*/) {
  FILE *f = fopen(filename, "rb");
  if (!f) return -1;

  if (fgetc(f) != 'B' || fgetc(f) != 'M') {
    fclose(f);
    return -1;
  }

  if (width != 0) {
    LONG biWidth;
    fseek(f, 18, SEEK_SET); // Seek to the width member of the header
    fread(&biWidth, sizeof(LONG), 1, f);
    if (width != biWidth) {
      fclose(f);
      return -1;
    }
  }

  if (height != 0) {
    LONG biHeight;
    fseek(f, 22, SEEK_SET); // Seek to the height member of the header
    fread(&biHeight, sizeof(LONG), 1, f);
    if (height != abs(biHeight)) {
      fclose(f);
      return -1; // Bitmap height can be negative too...
    }
  }

  DWORD dwSize;
  fseek(f, 2, SEEK_SET);
  fread(&dwSize, sizeof(DWORD), 1, f);
  dwSize -= 14;

  unsigned char* bitmap = (unsigned char*)malloc(dwSize);
  if (!bitmap) throw bad_alloc();

  fseek(f, 14, SEEK_SET);
  if (fread(bitmap, 1, dwSize, f) != dwSize) {
    fclose(f);
    return -1;
  }
  fclose(f);

  re->UpdateResource(RT_BITMAP, MAKEINTRESOURCE(id), MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), bitmap, dwSize);

  return 0;
}

// Added by Amir Szekely 8th July 2002
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

// Added by Amir Szekely 8th July 2002
// replace_icon, must get an initialized resource editor
// return values:
//   0  - All OK
//   -1 - Bad icon file
int replace_icon(CResourceEditor* re, WORD wIconId, char* filename)
{
  FILE* f = fopen(filename, "rb");
  if (!f) return -1;

  IconGroupHeader igh;
  fread(&igh, sizeof(IconGroupHeader), 1, f);

  if (igh.wIsIcon != 1 && igh.wReserved != 0) return -1;

  BYTE* rsrcIconGroup = (BYTE*)malloc(sizeof(IconGroupHeader) + igh.wCount*SIZEOF_RSRC_ICON_GROUP_ENTRY);
  if (!rsrcIconGroup) throw bad_alloc();

  CopyMemory(rsrcIconGroup, &igh, sizeof(IconGroupHeader));

  RsrcIconGroupEntry* ige = (RsrcIconGroupEntry*)(rsrcIconGroup + sizeof(IconGroupHeader));

  int iNewIconSize = 0;

  for (int i = 0; i < igh.wCount; i++) {
    fread(ige, sizeof(FileIconGroupEntry)-sizeof(DWORD), 1, f);
    ige->wRsrcId = i+1;

    DWORD dwOffset;
    fread(&dwOffset, sizeof(DWORD), 1, f);

    fpos_t pos;
    fgetpos(f, &pos);

    if (fseek(f, dwOffset, SEEK_SET)) return -1;
    BYTE* iconData = (BYTE*)malloc(ige->dwRawSize);
    if (!iconData) throw bad_alloc();
    fread(iconData, sizeof(BYTE), ige->dwRawSize, f);
    re->UpdateResource(RT_ICON, MAKEINTRESOURCE(i+1), MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), iconData, ige->dwRawSize);
    free(iconData);

    fsetpos(f, &pos);

    // Every icon entry should be 8 aligned
    iNewIconSize += ((ige->dwRawSize%8 == 0) ? ige->dwRawSize : ige->dwRawSize - (ige->dwRawSize%8) + 8);

    // Seems like the compiler refuses to increase the pointer by just 14.
    // If you'll replace this line by ige++ you will get unwanted results.
    ige = (RsrcIconGroupEntry*)((BYTE*)ige + SIZEOF_RSRC_ICON_GROUP_ENTRY);
  }

  fclose(f);

  re->UpdateResource(RT_GROUP_ICON, MAKEINTRESOURCE(wIconId), MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), rsrcIconGroup, sizeof(IconGroupHeader) + igh.wCount*SIZEOF_RSRC_ICON_GROUP_ENTRY);

  icondata_size = iNewIconSize;

  return 0;
}

// Added by Amir Szekely 8th July 2002
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon that should replace the installer icon data
// return values:
//   0  - Bad icon file
//   Anything else - Pointer to the uninstaller icon data
unsigned char* generate_uninstall_icon_data(char* filename)
{
  int i;

  FILE* f = fopen(filename, "rb");
  if (!f) return 0;

  IconGroupHeader igh;
  if (!fread(&igh, sizeof(IconGroupHeader), 1, f)) return 0;

  if (igh.wIsIcon != 1 && igh.wReserved != 0) return 0;

  int iNewIconSize = 0;
  FileIconGroupEntry ige;

  DWORD* offsets = (DWORD*)malloc(sizeof(DWORD)*igh.wCount);
  DWORD* rawSizes = (DWORD*)malloc(sizeof(DWORD)*igh.wCount);
  if (!offsets || !rawSizes) throw bad_alloc();

  for (i = 0; i < igh.wCount; i++) {
    if (!fread(&ige, sizeof(FileIconGroupEntry), 1, f)) return 0;
    offsets[i] = ige.dwImageOffset;
    rawSizes[i] = ige.dwRawSize;
    iNewIconSize += ige.dwRawSize;
  }

  // First DWORD tells how many icons this array contains
  // Before each icon come two DWORDs, one for size and the other for offset (set later)
  iNewIconSize += sizeof(DWORD)*(1 + igh.wCount*2);

  BYTE* pbUninstIcon = (BYTE*)malloc(iNewIconSize);
  if (!pbUninstIcon) throw bad_alloc();

  BYTE* seeker = pbUninstIcon;

  *(DWORD*)seeker = igh.wCount;
  seeker += sizeof(DWORD);

  for (i = 0; i < igh.wCount; i++) {
    *(DWORD*)seeker = rawSizes[i];
    seeker += sizeof(DWORD);
    *(DWORD*)seeker = 0;
    seeker += sizeof(DWORD);
    fseek(f, offsets[i], SEEK_SET);
    fread(seeker, 1, rawSizes[i], f);
    seeker += rawSizes[i];
  }

  free(offsets);
  free(rawSizes);

  unicondata_size = iNewIconSize;

  return pbUninstIcon;
}

// Added by Amir Szekely 11th July 2002
#define MY_ASSERT(x, y) if (x) {if (g_display_errors) fprintf(g_output,"\ngenerate_unicons_offsets: %s -- failing!\n", y);return 0;}

int find_in_dir(PRESOURCE_DIRECTORY rd, WORD id) {
  for (int i = rd->Header.NumberOfNamedEntries; i < rd->Header.NumberOfNamedEntries + rd->Header.NumberOfIdEntries; i++) {
    if (rd->Entries[i].Id == id) {
      return i;
    }
  }
  return -1;
}

// Fill the array of icons for uninstall with their offsets
// Returns 0 if failed, anything else is icon_offset.
int generate_unicons_offsets(unsigned char* exeHeader, unsigned char* uninstIconData) {
  int i;

  MY_ASSERT(PIMAGE_DOS_HEADER(exeHeader)->e_magic != IMAGE_DOS_SIGNATURE, "invalid dos header");

  PIMAGE_NT_HEADERS ntHeaders = PIMAGE_NT_HEADERS(exeHeader + PIMAGE_DOS_HEADER(exeHeader)->e_lfanew);

  MY_ASSERT(ntHeaders->Signature != IMAGE_NT_SIGNATURE, "invalid nt headers");

  DWORD dwResourceSectionVA = ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
  PIMAGE_SECTION_HEADER sectionHeadersArray = IMAGE_FIRST_SECTION(ntHeaders);

  for (i = 0; i < ntHeaders->FileHeader.NumberOfSections; i++)
		if (dwResourceSectionVA == sectionHeadersArray[i].VirtualAddress)
			break;

  MY_ASSERT(i == ntHeaders->FileHeader.NumberOfSections, "can't find resource section");

  PRESOURCE_DIRECTORY rdRoot = PRESOURCE_DIRECTORY(exeHeader + sectionHeadersArray[i].PointerToRawData);

  int idx = find_in_dir(rdRoot, WORD(RT_ICON));
  MY_ASSERT(idx == -1, "no icons?!");
  MY_ASSERT(!rdRoot->Entries[idx].DataIsDirectory, "bad resource directory");

  PRESOURCE_DIRECTORY rdIcons = PRESOURCE_DIRECTORY(rdRoot->Entries[idx].OffsetToDirectory + DWORD(rdRoot));

  unsigned char* seeker = uninstIconData;
  MY_ASSERT(*(DWORD*)seeker != rdIcons->Header.NumberOfIdEntries, "number of icons doesn't match");
  seeker += sizeof(DWORD);

  for (i = 0; i < rdIcons->Header.NumberOfIdEntries; i++) { // Icons dir can't have named entries
    MY_ASSERT(!rdIcons->Entries[i].DataIsDirectory, "bad resource directory");
    PRESOURCE_DIRECTORY rd = PRESOURCE_DIRECTORY(rdIcons->Entries[i].OffsetToDirectory + DWORD(rdRoot));
    MY_ASSERT(rd->Entries[0].DataIsDirectory, "bad resource directory");
    PIMAGE_RESOURCE_DATA_ENTRY rde = PIMAGE_RESOURCE_DATA_ENTRY(rd->Entries[0].OffsetToData + DWORD(rdRoot));

    DWORD dwSize = *(DWORD*)seeker;
    seeker += sizeof(DWORD);
    MY_ASSERT(dwSize != rde->Size, "installer, uninstaller icon size mismatch");
    // Set offset
    *(DWORD*)seeker = rde->OffsetToData + DWORD(rdRoot) - dwResourceSectionVA - DWORD(exeHeader);
    seeker += sizeof(DWORD) + dwSize;
  }
  MY_ASSERT(i == 0, "no icons found");

  return PIMAGE_RESOURCE_DATA_ENTRY(PRESOURCE_DIRECTORY(rdIcons->Entries[0].OffsetToDirectory + DWORD(rdRoot))->Entries[0].OffsetToData + DWORD(rdRoot))->OffsetToData + DWORD(rdRoot) - dwResourceSectionVA - DWORD(exeHeader);
}
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT
