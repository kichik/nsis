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
              char *dll = new char[lstrlen(dllName)];
              lstrcpy(dll, dllName);
              m_storedDLLs.push_back(dll);
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
  return GetPluginDll(token) ? true : false;
}

char* Plugins::GetPluginDll(char* command)
{
  bool malloced = false;
  char *colons = strstr(command,"::");
  if (!colons) return 0;

  *colons = 0;

  char *p = command;

  while (*p != '.' && *p) p++;

  if (lstrcmpi(p, ".dll")) {
    char *new_command = (char *)malloc(lstrlen(command)+1+4);
    wsprintf(new_command, "%s.dll::%s", command, colons+2);
    command = new_command;
    malloced = 1;
  }
  
  *colons = ':';
  
  char *result = m_commands.find(command);
  if (malloced) free(command);
  return result;
}

void Plugins::StoreInstDLL(char* dllName)
{
  for (int i = 0; i < m_installDLLs.size(); i++)
    if (!strcmp(m_installDLLs[i], dllName))
      return;
  m_installDLLs.push_back(strdup(dllName));
}

void Plugins::StoreUninstDLL(char* dllName)
{
  for (int i = 0; i < m_uninstallDLLs.size(); i++)
    if (!strcmp(m_uninstallDLLs[i], dllName))
      return;
  m_uninstallDLLs.push_back(strdup(dllName));
}

char* Plugins::GetInstDLL(int i)
{
  if (i >= 0 && i < m_installDLLs.size()) return m_installDLLs[i];
  else return 0;
}

char* Plugins::GetUninstDLL(int i)
{
  if (i >= 0 && i < m_uninstallDLLs.size()) return m_uninstallDLLs[i];
  else return 0;
}

#endif