

#include "Plugins.h"
#include <windows.h>


extern FILE *g_output;


struct COFFHeader
{
    unsigned short Machine;
    unsigned short NumberOfSections;
    unsigned long  TimeDateStamp;
    unsigned long  PointerToSymbolTable;
    unsigned long  NumberOfSymbols;
    unsigned short SizeOfOptionalHeader;
    unsigned short Characteristics;
};
const int COFFHeaderSize = 20;

struct StandardHeader
{
    unsigned short Magic;
    unsigned char  MajorLinkerVersion;
    unsigned char  MinorLinkerVersion;
    unsigned long  SizeOfCode;
    unsigned long  SizeOfInitializedData;
    unsigned long  SizeOfUninitializedData;
    unsigned long  AddressOfEntryPoint;
    unsigned long  BaseOfCode;
    unsigned long  BaseOfData;                  // PE32+
};
const int StandardHeaderSize = 24;
const int StandardHeaderSizePlus = 28;

struct WindowsSpecificFields
{
    unsigned long  ImageBase;
    unsigned long  SectionAlignment;
    unsigned long  FileAlignment;
    unsigned short MajorOperatingSystemVersion;
    unsigned short MinorOperatingSystemVersion;
    unsigned short MajorImageVersion;
    unsigned short MinorImageVersion;
    unsigned short MajorSubsystemVersion;
    unsigned short MinorSubsystemVersion;
    unsigned long  Reserved;
    unsigned long  SizeOfImage;
    unsigned long  SizeOfHeaders;
    unsigned long  CheckSum;
    unsigned short Subsystem;
    unsigned short DllCharacteristics;
    unsigned long  SizeOfStackReserve;
    unsigned long  SizeOfStackCommit;
    unsigned long  SizeOfHeapReserve;
    unsigned long  SizeOfHeapCommit;
    unsigned long  LoaderFlags;                 // Obsolete
    unsigned long  NumberOfRvaAndSizes;
};
struct WindowsSpecificFieldsPlus
{
    unsigned long  ImageBase;
    unsigned long  ImageBasePlus;               // PE32+
    unsigned long  SectionAlignment;
    unsigned long  FileAlignment;
    unsigned short MajorOperatingSystemVersion;
    unsigned short MinorOperatingSystemVersion;
    unsigned short MajorImageVersion;
    unsigned short MinorImageVersion;
    unsigned short MajorSubsystemVersion;
    unsigned short MinorSubsystemVersion;
    unsigned long  Reserved;
    unsigned long  SizeOfImage;
    unsigned long  SizeOfHeaders;
    unsigned long  CheckSum;
    unsigned short Subsystem;
    unsigned short DllCharacteristics;
    unsigned long  SizeOfStackReserve;
    unsigned long  SizeOfStackReservePlus;      // PE32+
    unsigned long  SizeOfStackCommit;
    unsigned long  SizeOfStackCommitPlus;       // PE32+
    unsigned long  SizeOfHeapReserve;
    unsigned long  SizeOfHeapReservePlus;       // PE32+
    unsigned long  SizeOfHeapCommit;
    unsigned long  SizeOfHeapCommitPlus;        // PE32+
    unsigned long  LoaderFlags;                 // Obsolete
    unsigned long  NumberOfRvaAndSizes;
};
const int WindowsHeaderSize = 68;
const int WindowsHeaderSizePlus = 88;

struct DataDirectory
{
    unsigned long Rva;
    unsigned long Size;
};
const int DataDirectorySize = 8;

struct SectionHeader
{
    unsigned char  Name[8];                     // null terminated ONLY if
    unsigned long  VirtualSize;                 // all 8 bytes are NOT used
    unsigned long  VirtualAddress;
    unsigned long  SizeOfRawData;
    unsigned long  PointerToRawData;
    unsigned long  PointerToRelocations;
    unsigned long  PointerToLineNumbers;
    unsigned short NumberOfRelocations;
    unsigned short NumberOfLineNumbers;
    unsigned long  Characteristics;
};
const int SectionHeaderSize = 40;

struct ExportTable
{
    unsigned long  ExportFlags;
    unsigned long  TimeDateStamp;
    unsigned short MajorVersion;
    unsigned short MinorVersion;
    unsigned long  NameRVA;
    unsigned long  OrdinalBase;
    unsigned long  AddressTableEntries;
    unsigned long  NumberOfNamePointers;
    unsigned long  ExportAddressTableRVA;
    unsigned long  NamePointerRVA;
    unsigned long  OrdinalTableRVA;
};

#define IMAGE_FILE_DLL 0x2000

enum
{
    EXPORT, IMPORT, RESOURCE, EXCEPTION, CERTIFICATES, DEBUG, BASERELOCATION,
    ARCHITECTURE, SPECIAL, THREADSTORAGE, LOADCONFIG, BOUND, IMPORTADDRESS, 
    DELAYIMPORT, RESERVED1, RESERVED2
};


void Plugins::FindCommands(char* path,bool displayInfo)
{
  if (path)
  {
    int length = strlen(path);

    if (length > 0)
    {
      WIN32_FIND_DATA data;
      HANDLE handle;
      
      if (path[length-1] == '\\' ||
          path[length-1] == '/')
      {
        length--;
      }

      char* basePath = new char [length+1];
      strncpy(basePath,path,length);
      basePath[length] = 0;

      char* pathAndWildcard = new char [length+7];
      strcpy(pathAndWildcard,basePath);
      strcat(pathAndWildcard,"\\*.dll");

      handle = FindFirstFile(pathAndWildcard,&data);
      if (handle != INVALID_HANDLE_VALUE)
      {
        do
        {
          char* dllPath = new char [length+strlen(data.cFileName)+2];
          strcpy(dllPath,basePath);
          strcat(dllPath,"\\");
          strcat(dllPath,data.cFileName);
          GetExports(dllPath,displayInfo);
          delete[] dllPath;
        } while (FindNextFile(handle,&data));
      }

      delete[] pathAndWildcard;
      delete[] basePath;
    }
  }
}

void Plugins::GetExports(char* pathToDll,bool displayInfo)
{
  if (pathToDll)
  {
    unsigned char* dlldata    = 0;
    long           dlldatalen = 0;
    bool           loaded     = false;
    char           dllName[100];
    char           signature[256];

    dllName[0] = 0;
    char* ptr = strrchr(pathToDll,'\\');
    if (ptr && *ptr && *(ptr+1)) strcpy(dllName,ptr+1);

    FILE* dll = fopen(pathToDll,"rb");
    if (dll)
    {
      fseek(dll,0,SEEK_END);
      dlldatalen = ftell(dll);
      fseek(dll,0,SEEK_SET);
      if (dlldatalen > 0)
      {
        dlldata = new unsigned char [dlldatalen];
        if (dlldata)
        {
          size_t bytesread = fread((void*)dlldata,1,dlldatalen,dll);
          if ((long)bytesread == dlldatalen)
            loaded = true;
        }
      }
      fclose(dll);
    }

    if (!loaded)
    {
      if (dlldata) delete[] dlldata;
      return;
    }

    // read the file offset stored at 0x3c (a single byte)
    // then find the pe signature bytes stored at that offset
    unsigned long* peheaderoffset = (unsigned long*) &dlldata[0x3c];
    unsigned char pesignature[4] = {
      dlldata[(*peheaderoffset)+0],
      dlldata[(*peheaderoffset)+1],
      dlldata[(*peheaderoffset)+2],
      dlldata[(*peheaderoffset)+3],
    };

    if (pesignature[0] == 'P' &&
        pesignature[1] == 'E' &&
        pesignature[2] == '\0' &&
        pesignature[3] == '\0')
    {
      // after the signature comes the COFF header
      COFFHeader* coffHeader = (COFFHeader*)&dlldata[(*peheaderoffset)+4];

      if (coffHeader->Characteristics & IMAGE_FILE_DLL)
      {
        // after the COFF header comes the Optional Header magic number
        // (two bytes)
        unsigned char ohmagicnumber[2] = {
          dlldata[(*peheaderoffset)+4+COFFHeaderSize+0],
          dlldata[(*peheaderoffset)+4+COFFHeaderSize+1]
        };

        // 0x10b means a PE header, but 0x20b means a PE+ header
        // not sure if I need to care yet.
        if ((ohmagicnumber[0] == 0x0b  &&
             ohmagicnumber[1] == 0x01) ||
            (ohmagicnumber[0] == 0x0b  &&
             ohmagicnumber[1] == 0x02))
        {
          bool plus = (ohmagicnumber[0] == 0x0b && 
                       ohmagicnumber[1] == 0x02);

          if (!plus)
          {
            const int standardHeaderSize         = (plus ? StandardHeaderSizePlus : StandardHeaderSize);
            const int windowsHeaderSize          = (plus ? WindowsHeaderSizePlus : WindowsHeaderSize);
      
            int optionalHeaderOffset             = (*peheaderoffset)+4+COFFHeaderSize;
            StandardHeader* standardHeader       = (StandardHeader*)&dlldata[optionalHeaderOffset];
            WindowsSpecificFields* windowsHeader = (WindowsSpecificFields*)&dlldata[optionalHeaderOffset+standardHeaderSize+4];
            DataDirectory* directories           = (DataDirectory*)&dlldata[optionalHeaderOffset+standardHeaderSize+windowsHeaderSize+4];

            DataDirectory* exportHeader = directories;
            SectionHeader* sectionTable = 
              (SectionHeader*) &dlldata[
                  (*peheaderoffset)
                + 4
                + COFFHeaderSize
                + coffHeader->SizeOfOptionalHeader];

            SectionHeader* section = sectionTable;
            for (unsigned long i = 0; i < coffHeader->NumberOfSections; i++)
            {
              DataDirectory* directory = directories;
              for (unsigned long i = 0; i < windowsHeader->NumberOfRvaAndSizes; i++)
              {
                  if (directory->Rva >= section->VirtualAddress &&
                     (directory->Rva+directory->Size) <= (section->VirtualAddress+section->VirtualSize) &&
                      i == EXPORT)
                  {
                    unsigned char* ptr   = dlldata+section->PointerToRawData;
                    ExportTable* exports = (ExportTable*)(dlldata+section->PointerToRawData+(directory->Rva-section->VirtualAddress));

                    // find the start of the name table
                    unsigned long* nameTableEntry = (unsigned long*)(ptr+exports->NamePointerRVA-section->VirtualAddress);

                    // walk the name table
                    for (unsigned long i = 0; i < exports->NumberOfNamePointers; i++)
                    {
                      char* namePointer = (char*)(ptr+(*nameTableEntry)-section->VirtualAddress);
                      strcpy(signature,dllName);
                      strcat(signature,"::");
                      strcat(signature,namePointer);
                      m_commands.add(signature,pathToDll);
                      if (displayInfo)
                        fprintf(g_output," - %s\n",signature);
                      nameTableEntry++;
                    }
                  }

                  directory++;
              }
            
              section++;
            }
          }
        }
      }
    }

    delete[] dlldata;
  }
}

bool Plugins::IsPluginCommand(char* token)
{
  return GetPluginDll(token) ? true : false;
}

char* Plugins::GetPluginDll(char* command)
{
  if (strstr(command,"::"))
    return m_commands.find(command);

  // slow & stupid but it doesn't matter
  int i = 0,pos = 0;
  char* signatures = m_commands.defines.get();
  while (pos != -1)
  {
    pos = m_commands.defines.idx2pos(i++);
    if (pos >= 0)
    {
      char* cmd = strstr(signatures+pos,"::");
      if (cmd && strcmp(cmd+2,command) == 0)
        return m_commands.find(signatures+pos);
    }
  }

  return 0;
}

void Plugins::StoreDllDataHandle(char* signature,int handle)
{
  int idx = -1;
  m_commands.defines.find(signature,0,&idx);
  if (idx > -1)
  {
    m_dataHandles.reserve(idx+1);
    m_dataHandles[idx] = handle;
  }
}

int Plugins::GetDllDataHandle(char* signature)
{
  int idx = -1;
  if (-1 != m_commands.defines.find(signature,0,&idx))
    return m_dataHandles[idx];
  return -1;
}