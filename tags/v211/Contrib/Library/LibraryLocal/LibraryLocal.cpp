/*

  LibraryLocal - used by the Library.nsh macros
  Get the version of local DLL and TLB files
  Written by Joost Verburg

*/

#include <windows.h>
#include <stdio.h>
#include <iostream>
#include <fstream>

using namespace std;

int main(int argc, char* argv[])
{

  // Parse the command line

	string cmdline;

	string mode;
	string filename;
	string filepath;

	int filefound = 0;

  if (argc != 4)
    return 1;

  // Get the full path of the local file

  mode = argv[1];
  filename = argv[2];

  char buf[MAX_PATH];
  GetCurrentDirectory(MAX_PATH, buf);
  filepath = buf;

  if ((filename.substr(0, 1).compare("\\") != 0) && (filename.substr(1, 1).compare(":") != 0)) {
    
    // Path is relative

    if (filepath.substr(filepath.length() - 1, 1).compare("\\") != 0)
      filepath.append("\\");
      
    filepath.append(filename);

  } else if ((filename.substr(0, 1).compare("\\") == 0) && (filename.substr(1, 1).compare("\\") != 0)) {

    // Path is relative to current root

    if (filepath.substr(1, 1).compare(":") == 0) {

      // Standard path

      filepath = filepath.substr(0, filepath.find('\\'));
      filepath.append(filename);

    } else {

      // UNC path
      
      filepath = filepath.substr(0, filepath.find('\\', filepath.find('\\', 2) + 1));
      filepath.append(filename);
        
    }

  } else {
    
    // Absolute path

    filepath = filename;

  }

  // Validate filename

  WIN32_FIND_DATA wfd;
  HANDLE hFind = FindFirstFile(filepath.c_str(), &wfd);
  
  if (hFind != INVALID_HANDLE_VALUE)
  {
    filefound = 1;
    FindClose(hFind);
  }
	
	int versionfound = 0;
	DWORD low = 0, high = 0;

	if (filefound)
	{

		// Get version
		
		// DLL
		
		if (mode.compare("D") == 0)
		{
			
			DWORD versionsize;
			DWORD temp;
			  
			versionsize = GetFileVersionInfoSize((char*)filepath.c_str(), &temp);
			
			if (versionsize)
			{
			  
				void *buf;
				buf = (void *)GlobalAlloc(GPTR, versionsize);
			  
				if (buf)
				{
				
					UINT uLen;
					VS_FIXEDFILEINFO *pvsf;

					if (GetFileVersionInfo((char*)filepath.c_str(), 0, versionsize, buf) && VerQueryValue(buf, "\\", (void**)&pvsf,&uLen))
					{
						high = pvsf->dwFileVersionMS;
						low = pvsf->dwFileVersionLS;

						versionfound = 1;
					} 

					GlobalFree(buf);

				}

			}

		}

		// TLB
		
		if (mode.compare("T") == 0)
		{
			
			wchar_t ole_filename[1024];
			MultiByteToWideChar(CP_ACP, 0, filepath.c_str(), filepath.length() + 1, ole_filename, 1024);
			
			ITypeLib* typeLib;
			HRESULT hr;
			
			hr = LoadTypeLib(ole_filename, &typeLib);
			
			if (SUCCEEDED(hr)) {

				TLIBATTR* typelibAttr;
				
				hr = typeLib->GetLibAttr(&typelibAttr);

				if (SUCCEEDED(hr)) {
					
					high = typelibAttr->wMajorVerNum;
					low = typelibAttr->wMinorVerNum;
					
					versionfound = 1;

				}

				typeLib->Release();

			}

		}

	}

	// Write the version to an NSIS header file

	ofstream header(argv[3], ofstream::out);
	
	if (header)
	{

		if (!filefound)
		{
			header << "!define LIBRARY_VERSION_FILENOTFOUND" << endl;
		}
		else if (!versionfound)
		{
			header << "!define LIBRARY_VERSION_NONE" << endl;
		}
		else
		{
			header << "!define LIBRARY_VERSION_HIGH " << high << endl;
			header << "!define LIBRARY_VERSION_LOW " << low << endl;
		}
	  
	  header.close();

	}

	return 0;

}
