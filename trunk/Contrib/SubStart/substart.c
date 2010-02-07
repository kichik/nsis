/*
 * substart.c
 *
 * Copyright (c) 2010 Thomas Gaugler
 *
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 *
 * Licence details can be found in the file COPYING.
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include <windows.h>
#include <tchar.h>
#include <stdio.h>

/* Macro to determine the string length of a constant */
#define CONST_STRLEN(x) ((sizeof(x) / sizeof(*x)) - 1)

/* Name of the sub folder containing the executable
   invoked by this stub executable.
 */
static const TCHAR SUBFOLDER[] = _T("Bin");

/* Redirect to an executable located in a sub folder relative to
   this starter executable. Name of the stub executable has to
   match with the one in the sub folder.
   The command line parameters are passed along.
 */
int main( int argc, char *argv[] )
{
    int err = ERROR_SUCCESS;
    TCHAR szPath[MAX_PATH];

    /* Get the full path of the running executable */
    if ( GetModuleFileName( NULL, szPath, MAX_PATH ) )
    {
        size_t len = _tcslen(szPath);
        size_t offset = len + CONST_STRLEN(SUBFOLDER) + 1;

        err = ERROR_BAD_PATHNAME;
        if (offset < MAX_PATH)
        {
             /* Move file name part of full path by length of sub folder
                name and thereafter fill in the sub folder name in the
                newly created gap.
              */
             register TCHAR *p = szPath + len;
             register TCHAR *q = szPath + offset;

             while (p > szPath)
             {
                *(q--) = *(p--);
                if (*p == '\\')
                {
                    /* Fill in sub folder name */
                    *q = *p;
                    q = ++p;
                    p = (TCHAR *)SUBFOLDER;
                    while (*p)
                    {
                       *(q++) = *(p++);
                    }
                    err = ERROR_SUCCESS;
                    break;
                }
             }   
        }
       
        if (err) 
        {
            _tprintf( _T("Path too long: %s\n"), szPath );
        }
        else
        {
            STARTUPINFO si;
            PROCESS_INFORMATION pi;

            ZeroMemory( &si, sizeof(si) );
            si.cb = sizeof(si);
            ZeroMemory( &pi, sizeof(pi) );

            /* Start a subprocess running the executable of the 
               sub folder of the cuand wait for its completion */
            if ( CreateProcess( szPath, GetCommandLine(), NULL, NULL,
                FALSE, 0, NULL, NULL, &si, &pi ) )
            {
                WaitForSingleObject( pi.hProcess, INFINITE );
                CloseHandle( pi.hProcess );
                CloseHandle( pi.hThread );
            }
            else
            {
                err = GetLastError();
                _tprintf( _T("CreateProcess (%s) failed (%d).\n"), szPath, err );
            }
        }
    }
    else
    {
        err = GetLastError();
        _tprintf( _T("GetModuleFileName failed (%d).\n"), err );
    }

    return err;
}

