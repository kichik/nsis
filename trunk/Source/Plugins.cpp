#include "exehead/config.h"
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT

#include "Plugins.h"
#include <windows.h>
#include <WinNT.h>


extern FILE *g_output;

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
          wsprintf(dllPath,"%s\\%s",basePath,data.cFileName);
          GetExports(dllPath,displayInfo);
          delete[] dllPath;
        } while (FindNextFile(handle,&data));
      }

      delete[] pathAndWildcard;
      delete[] basePath;
    }

    m_dataHandles.resize(m_funcsCount*sizeof(int));
    m_uninstDataHandles.resize(m_funcsCount*sizeof(int));
    memset(m_dataHandles.get(), -1, m_funcsCount*sizeof(int));
    memset(m_uninstDataHandles.get(), -1, m_funcsCount*sizeof(int));
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
    ptr = strstr(dllName, ".dll");
    if (ptr) *ptr = 0;

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

    PIMAGE_NT_HEADERS NTHeaders = PIMAGE_NT_HEADERS(dlldata + PIMAGE_DOS_HEADER(dlldata)->e_lfanew);
    if (NTHeaders->Signature == IMAGE_NT_SIGNATURE)
    {
      if (NTHeaders->FileHeader.Characteristics & IMAGE_FILE_DLL)
      {
        if (NTHeaders->OptionalHeader.NumberOfRvaAndSizes <= IMAGE_DIRECTORY_ENTRY_EXPORT) return;

        DWORD ExportDirVA = NTHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
        DWORD ExportDirSize = NTHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
        PIMAGE_SECTION_HEADER sections = IMAGE_FIRST_SECTION(NTHeaders);

        for (int i = 0; i < NTHeaders->FileHeader.NumberOfSections; i++)
          {
          if (sections[i].VirtualAddress <= ExportDirVA
              && sections[i].VirtualAddress+sections[i].Misc.VirtualSize >= ExportDirVA+ExportDirSize)
            {
            PIMAGE_EXPORT_DIRECTORY exports = PIMAGE_EXPORT_DIRECTORY(dlldata + sections[i].PointerToRawData + ExportDirVA - sections[i].VirtualAddress);
            unsigned long *names = (unsigned long*)((char*)exports + exports->AddressOfNames - ExportDirVA);
            for (unsigned long j = 0; j < exports->NumberOfNames; j++)
            {
              char *name = (char*)exports + names[j] - ExportDirVA;
              wsprintf(signature, "%s::%s", dllName, name);
              m_commands.add(signature, pathToDll);
              m_funcsCount++;
              if (displayInfo)
                fprintf(g_output, " - %s\n", signature);
            }
            break;
          }
        }
      }
    }

    delete[] dlldata;
  }
}

bool Plugins::IsPluginCommand(char* token)
{
  return GetPluginDll(0, token, 0) ? true : false;
}

char* Plugins::GetPluginDll(int uninst, char* command, int* dataHandle)
{
  char* ret = m_commands.find(command, dataHandle);
  if (dataHandle && ret) {
    if (uninst) *dataHandle = ((int*)m_uninstDataHandles.get())[*dataHandle];
    else *dataHandle = ((int*)m_dataHandles.get())[*dataHandle];
  }
  return ret;
}

void Plugins::SetDllDataHandle(int uninst, char* command, int dataHandle)
{
  int idx = -1;
  m_commands.find(command, &idx);
  if (idx == -1) return;
  if (uninst) ((int*)m_uninstDataHandles.get())[idx] = dataHandle;
  else ((int*)m_dataHandles.get())[idx] = dataHandle;
}

#endif