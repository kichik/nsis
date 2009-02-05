// System.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "Plugin.h"
#include "Buffers.h"
#include "System.h"
#ifndef __GNUC__
#define _DECL_DLLMAIN   /* enable prototypes for DllMain and _CRT_INIT */
#include <process.h>
#include <crtdbg.h>
#else
#define _RPT0(type, msg)
#define _CRT_WARN           0
#endif /* __GNUC__ */
#include <objbase.h>

// Type conversion macro
#define INT_TO_POINTER(i) ((void *) (int) (i))

// Parse Section Type 
#define PST_PROC    0
#define PST_PARAMS  1
#define PST_RETURN  2
#define PST_OPTIONS 3

#define PCD_NONE    0
#define PCD_PROC    1
#define PCD_PARAMS  2
#define PCD_DONE    3   // Just Continue

const int ParamSizeByType[7] = {0, // PAT_VOID (Size will be equal to 1)
    1, // PAT_INT
    2, // PAT_LONG
    1, // PAT_STRING
    1, // PAT_WSTRING
    1, // PAT_GUID
    0}; // PAT_CALLBACK (Size will be equal to 1)

int LastStackPlace;
int LastStackReal;
DWORD LastError;
volatile SystemProc *LastProc;
int CallbackIndex;
HINSTANCE g_hInstance;

// Return to callback caller with stack restore
char retexpr[4];
HANDLE retaddr;

char *GetResultStr(SystemProc *proc)
{
    char *buf = AllocString();
    if (proc->ProcResult == PR_OK) lstrcpy(buf, "ok");
    else if (proc->ProcResult == PR_ERROR) lstrcpy(buf, "error");
    else if (proc->ProcResult == PR_CALLBACK) wsprintf(buf, "callback%d", proc->CallbackIndex);
    return buf;
}

#ifdef SYSTEM_LOG_DEBUG

// System log debugging turned on
#define SYSTEM_LOG_ADD(a)  { register int _len = lstrlen(syslogbuf); lstrcpyn(syslogbuf + _len, a, sizeof(syslogbuf) - _len); }
#define SYSTEM_LOG_POST     { SYSTEM_LOG_ADD("\n"); WriteToLog(syslogbuf); *syslogbuf = 0; }

HANDLE logfile = NULL;
char syslogbuf[4096] = "";
int logop = 0;

void WriteToLog(char *buffer)
{
    DWORD written;
    char timebuffer[128];

    GetTickCount();

    if (logfile == NULL) return;

    SetFilePointer(logfile, 0, 0, FILE_END);

    wsprintf(timebuffer, "%04d  %04d.%03d    ", (++logop)%10000, (GetTickCount() / 1000) % 10000,
        GetTickCount() % 1000);

    _RPT0(_CRT_WARN, timebuffer);
    _RPT0(_CRT_WARN, buffer);

    WriteFile(logfile, timebuffer, lstrlen(timebuffer), &written, NULL);
    WriteFile(logfile, buffer, lstrlen(buffer), &written, NULL);
//    FlushFileBuffers(logfile);
}

PLUGINFUNCTION(Debug)
{
    char *o1;
    o1 = system_popstring();

    if (logfile == NULL)
        if (lstrlen(o1) > 0)
        {
            SYSTEMTIME t;
            char buffer[1024], buftime[1024], bufdate[1024];

            // Init debugging
            logfile = CreateFile(o1, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, 
                OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);

            SetFilePointer(logfile, 0, 0, FILE_END);

            logop = 0;
            GetLocalTime(&t);
            GetTimeFormat(LOCALE_SYSTEM_DEFAULT, LOCALE_NOUSEROVERRIDE, &t, NULL, buftime, 1024);
            GetDateFormat(LOCALE_SYSTEM_DEFAULT, LOCALE_NOUSEROVERRIDE, &t, NULL, bufdate, 1024);
            wsprintf(buffer, "System, %s %s [build "__TIME__" "__DATE__"]\n", buftime, bufdate);
            WriteToLog(buffer);
        } else ;
    else
    if (lstrlen(o1) > 0)
    {
        // Log in to log file
        WriteToLog(o1);
    } else
    {
        // Stop debugging
        WriteToLog("Debug stopped.\n\n\n");
        CloseHandle(logfile);
        logfile = NULL;
    }
} PLUGINFUNCTIONEND

#else

// System log debugging turned off
#define SYSTEM_EVENT(a)
#define SYSTEM_LOG_ADD(a)
#define SYSTEM_LOG_POST

#endif

PLUGINFUNCTION(Get)
{
    SystemProc *proc = PrepareProc(FALSE);
    if (proc == NULL)
    {
      system_pushstring("error");
      return;
    }

    SYSTEM_LOG_ADD("Get ");
    SYSTEM_LOG_ADD(proc->DllName);
    SYSTEM_LOG_ADD("::");
    SYSTEM_LOG_ADD(proc->ProcName);
    SYSTEM_LOG_ADD("\n");
    SYSTEM_LOG_POST;
    if ((proc->Options & POPT_ALWRETURN) != 0)
    {
        // Always return flag set -> return separate proc and result
        system_pushint((int) proc);
        GlobalFree(system_pushstring(GetResultStr(proc)));
    } else
    {
        if (proc->ProcResult != PR_OK)
        {
            // No always return flag and error result - return result
            GlobalFree(system_pushstring(GetResultStr(proc)));
            // If proc is permanent?
            if ((proc->Options & POPT_PERMANENT) == 0)
                GlobalFree((HANDLE) proc); // No, free it
        }
        else // Ok result, return proc
            system_pushint((int) proc);
    }
} PLUGINFUNCTIONEND

PLUGINFUNCTION(Call)
{
    // Prepare input
    SystemProc *proc = PrepareProc(TRUE);
    if (proc == NULL)
      return;

    SYSTEM_LOG_ADD("Call ");
    SYSTEM_LOG_ADD(proc->DllName);
    SYSTEM_LOG_ADD("::");
    SYSTEM_LOG_ADD(proc->ProcName);
    SYSTEM_LOG_ADD("\n");
    if (proc->ProcResult != PR_CALLBACK)
        ParamAllocate(proc);
    ParamsIn(proc);

    // Make the call
    if (proc->ProcResult != PR_ERROR)
    {
        switch (proc->ProcType)
        {
        case PT_NOTHING:
            if (proc->ProcResult == PR_CALLBACK) 
                proc = CallBack(proc);
            break;
        case PT_PROC:
        case PT_VTABLEPROC:
            proc = CallProc(proc); break;
        case PT_STRUCT:
            CallStruct(proc); break;
        }
    }

    // Process output
    if ((proc->Options & POPT_ALWRETURN) != 0)
    {
        // Always return flag set - return separate return and result
        ParamsOut(proc);
        GlobalFree(system_pushstring(GetResultStr(proc)));
    } else
    {
        if (proc->ProcResult != PR_OK)
        {
            ProcParameter pp;
            // Save old return param
            pp = proc->Params[0];

            // Return result instead of return value
            proc->Params[0].Value = (int) GetResultStr(proc);
            proc->Params[0].Type = PAT_STRING;
            // Return all params
            ParamsOut(proc);

            // Restore old return param
            proc->Params[0] = pp;
        } else 
            ParamsOut(proc);        
    }

    if (proc->ProcResult != PR_CALLBACK)
    {
        // Deallocate params if not callback
        ParamsDeAllocate(proc);

        // if not callback - check for unload library option
        if ((proc->Options & POPT_UNLOAD) 
            && (proc->ProcType == PT_PROC) 
            && (proc->Dll != NULL)) 
            FreeLibrary(proc->Dll); // and unload it :)

        // In case of POPT_ERROR - first pop will be proc error
        if ((proc->Options & POPT_ERROR) != 0) system_pushint(LastError);
    }    

    // If proc is permanent?
    if ((proc->Options & POPT_PERMANENT) == 0)
        GlobalFree((HANDLE) proc); // No, free it
} PLUGINFUNCTIONEND

PLUGINFUNCTIONSHORT(Int64Op)
{
    __int64 i1, i2 = 0, i3, i4;
    char *op, *o1, *o2;
    char buf[128];

    // Get strings
    o1 = system_popstring(); op = system_popstring(); 
    i1 = myatoi64(o1); // convert first arg to int64
    if ((*op != '~') && (*op != '!'))
    {
        // get second arg, convert it, free it
        o2 = system_popstring();
        i2 = myatoi64(o2); 
        GlobalFree(o2);
    }

    // operation
    switch (*op)
    {
    case '+': i1 += i2; break;
    case '-': i1 -= i2; break;
    case '*': i1 *= i2; break;
    case '/': 
    case '%': 
        // It's unclear, but in this case compiler will use DivMod rountine
        // instead of two separate Div and Mod rountines.
        if (i2 == 0) { i3 = 0; i4 = i1; }
        else {i3 = i1 / i2; i4 = i1 % i2; }
        if (*op == '/') i1 = i3; else i1 = i4; 
        break;
    case '|': if (op[1] == '|') i1 = i1 || i2; else i1 |= i2; break;
    case '&': if (op[1] == '&') i1 = i1 && i2; else i1 &= i2; break;
    case '^': i1 ^= i2; break;
    case '~': i1 = ~i1; break;
    case '!': i1 = !i1; break;
    case '<': if (op[1] == '<') i1 = i1 << i2; else i1 = i1 < i2; break;
    case '>': if (op[1] == '>') i1 = i1 >> i2; else i1 = i1 > i2; break;
    case '=': i1 = (i1 == i2); break;
    }
    
    // Output and freedom
    myitoa64(i1, buf);
    system_pushstring(buf);
    GlobalFree(o1); GlobalFree(op);
} PLUGINFUNCTIONEND

__int64 GetIntFromString(char **p)
{
    char buffer[128], *b = buffer;
    (*p)++; // First character should be skipped
    while (((**p >= 'a') && (**p <= 'f')) || ((**p >= 'A') && (**p <= 'F')) || ((**p >= '0') && (**p <= '9')) || (**p == 'X') || (**p == '-') || (**p == 'x') || (**p == '|')) *(b++) = *((*p)++);
    *b = 0;
    (*p)--; // We should point at last digit
    return myatoi64(buffer);
}

SystemProc *PrepareProc(BOOL NeedForCall)
{
    int SectionType = PST_PROC, // First section is always proc spec
        ProcType = PT_NOTHING, // Default proc spec
        ChangesDone = 0,
        ParamIndex = 0,
        temp = 0, temp2, temp3, temp4;
    BOOL param_defined = FALSE;
    SystemProc *proc = NULL;
    char *ibuf, *ib, *sbuf, *cbuf, *cb;

    // Retrieve proc specs
    cb = (cbuf = AllocString()); // Current String buffer
    sbuf = AllocString(); // Safe String buffer
    ib = ibuf = system_popstring(); // Input string

    // Parse the string
    while (SectionType != -1)
    {
        // Check for Section Change
        BOOL changed = TRUE;
        ChangesDone = SectionType;

        if (SectionType != PST_PROC && proc == NULL)
            // no proc after PST_PROC is done means something is wrong with
            // the call syntax and we'll get a crash because everything needs
            // proc from here on.
            break;

        switch (*ib)
        {
        case 0x0: SectionType = -1; break;
        case '#': SectionType = PST_PROC; ProcType = PT_NOTHING; break;
        case '(': 
            SectionType = PST_PARAMS; 
            // fake-real parameter: for COM interfaces first param is Interface Pointer
            ParamIndex = ((ProcType == PT_VTABLEPROC)?(2):(1)); 
            temp3 = temp = 0;
            param_defined = FALSE;
            break;
        case ')': SectionType = PST_RETURN; temp3 = temp = 0; break;
        case '?': SectionType = PST_OPTIONS; temp = 1; break;
        default:
            changed = FALSE;
        }

        // Check for changes
        if (changed)
        {
            switch (ChangesDone)
            {
            case PST_PROC:
                *cb = 0;
                // Adopt proc
                if (proc == NULL)
                {
                    proc = (SystemProc *) GlobalAlloc(GPTR, sizeof(SystemProc));
                    proc->Options = 0;
                    proc->ParamCount = 0;
                }
                // Default parameters
                *proc->DllName = 0;
                *proc->ProcName = 0;
                proc->Dll = NULL;
                proc->Proc = NULL;
                proc->ProcType = ProcType;
                proc->ProcResult = PR_OK;
    
                // Section changed and previos section was Proc
                switch (ProcType)
                {
                case PT_NOTHING:
                    // Is it previous proc or just unknown proc?
                    if (cb != cbuf)
                    {
                        // Previous proc (for clear up)
                        SystemProc *pr = NULL;

                        if (proc != NULL) GlobalFree(proc);
                        // Get already defined proc                                      
                        proc = (SystemProc *) INT_TO_POINTER(myatoi64(cbuf));
                        if (!proc) break;

                        // Find the last clone at proc queue
                        while (proc && (proc->Clone != NULL)) proc = (pr = proc)->Clone;

                        // Clear parents record for child callback proc
                        if (pr != NULL) pr->Clone = NULL;

                        // Never Redefine?
                        if ((proc->Options & POPT_NEVERREDEF) != 0)
                        {
                            // Create new proc as copy
                            proc = GlobalCopy(proc);
                            // NeverRedef options is never inherited
                            proc->Options &= (~POPT_NEVERREDEF) & (~POPT_PERMANENT);
                        } else proc->Options |= POPT_PERMANENT; // Proc is old -> permanent
                    }
                    break;
                case PT_PROC:
                case PT_VTABLEPROC:
                    lstrcpy(proc->DllName, sbuf);
                case PT_STRUCT:
                    lstrcpy(proc->ProcName, cbuf);
                    break;
                }
                break;
            case PST_PARAMS:
                if (param_defined)
                    proc->ParamCount = ParamIndex;
                else
                    // not simply zero because of vtable calls
                    proc->ParamCount = ParamIndex - 1;
            case PST_RETURN:
            case PST_OPTIONS:
                break;
            }        
            ib++;
            cb = cbuf;
            continue;
        }

        // Parse the section
        ChangesDone = PCD_NONE;
        switch (SectionType)
        {
        // Proc sections parser
        case PST_PROC:
            switch (*ib)
            {
            case ':':
            case '-':
                // Is it '::'
                if ((*(ib) == '-') && (*(ib+1) == '>'))
                {
                    ProcType = PT_VTABLEPROC;    
                } else
                {
                    if ((*(ib+1) != ':') || (*(ib) == '-')) break;
                    ProcType = PT_PROC;
                }
                ib++; // Skip next ':'

                if (cb > cbuf)
                {
                    *cb = 0;
                    lstrcpy(sbuf, cbuf);
                } else  *sbuf = 0; // No dll - system proc
                
                // Ok
                ChangesDone = PCD_DONE;
                break;
            case '*':
                // Structure defenition
                ProcType = PT_STRUCT;
                ChangesDone = PCD_DONE;
                break;
            }          
            break;

        // Params and return sections parser
        case PST_RETURN:
            ParamIndex = 0; // Uses the same logic as PST_PARAMS section
        case PST_PARAMS:
            temp2 = -1; temp4 = 0; // Our type placeholder
            switch (*ib)
            {
            case ' ':
                break;
            case '_': // No param cutting specifier
                if (proc->ParamCount > ParamIndex) ParamIndex = proc->ParamCount;
                temp3 = temp = 0; // Clear parameter options
                if (proc->ParamCount != ((ProcType == PT_VTABLEPROC) ? 1 : 0))
                {
                  // only define params if the last count wasn't zero
                  // this prevents errornous param count for:
                  //   'user32::CloseClipboard()(_)'
                  // for vtable calls, param count should not be one
                  param_defined = TRUE;
                }
                break;
            case ',': // Next param
                temp3 = temp = 0; // Clear parameter options
                ParamIndex++;
                param_defined = TRUE;
                break;
            case '&':
                temp = 1; break; // Special parameter option
            case '*':
                temp = -1; break; // Pointer parameter option

            // Types
            case 'v':
            case 'V': temp2 = PAT_VOID; break;
            case 'i':
            case 'I': temp2 = PAT_INT; break;
            case 'l':
            case 'L': temp2 = PAT_LONG; break;
            case 'm':
            case 'M':
            case 't':
            case 'T': temp2 = PAT_STRING; break;
            case 'g':
            case 'G': temp2 = PAT_GUID; break;
            case 'w':
            case 'W': temp2 = PAT_WSTRING; break;
            case 'k':
            case 'K': temp2 = PAT_CALLBACK; break;

            // Input output specifiers
            case '.': temp3++; break; // skip specifier

            case 'R':
                temp4 = ((int) GetIntFromString(&ib))+1;
                if (temp4 < 11) temp4 += 10; 
                break;
            case 'r': temp4 = ((int) GetIntFromString(&ib))+1; break; // Register

            case '-':
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                // Numeric inline
                if (temp3 == 0)
                {
                    ib--;
                    // It's stupid, I know, but I'm too laze to do another thing
                    myitoa64(GetIntFromString(&(ib)),(char *)(temp4 = (int) AllocString()));
                }
                break;

            case '\"': case '\'': case '`':
                // Character inline
                {
                    char start = *ib;
                    cb = cbuf;
                    // copy inline
                    while (!((*(++ib) == start) && (*(ib+1) != start)) && (*ib))
                    {
                        if ((*ib) == start) ++ib;
                        *(cb++) = *(ib);
                    }
                    // finish and save
                    *cb = 0; 
                    temp4 = (int) AllocStr(cbuf);
                }
                break;

            case 's':
            case 'S': temp4 = -1; break;    // Stack
            case 'c':
            case 'C': temp4 = INST_CMDLINE+1; break;
            case 'd':
            case 'D': temp4 = INST_INSTDIR+1; break;
            case 'o':
            case 'O': temp4 = INST_OUTDIR+1; break;
            case 'e':
            case 'E': temp4 = INST_EXEDIR+1; break;
            case 'a':
            case 'A': temp4 = INST_LANG+1; break;
            }

            // Param type changed?
            if (temp2 != -1)
            {
                param_defined = TRUE;
                proc->Params[ParamIndex].Type = temp2;
                proc->Params[ParamIndex].Size = // If pointer, then 1, else by type
                    (temp == -1)?(1):((ParamSizeByType[temp2]>0)?(ParamSizeByType[temp2]):(1));
                // Get the parameter real special option value
                if (temp == 1) temp = ((int) GetIntFromString(&ib)) + 1;
                proc->Params[ParamIndex].Option = temp;
                proc->Params[ParamIndex].Value = 0;
                proc->Params[ParamIndex].Input = IOT_NONE;
                proc->Params[ParamIndex].Output = IOT_NONE;
            }
          
            // Param source/dest changed?
            if (temp4 != 0)
            {
                param_defined = TRUE;
                if (temp3 == 0)
                {
                    // it may contain previous inline input
                    if (!((proc->Params[ParamIndex].Input > -1) && (proc->Params[ParamIndex].Input <= __INST_LAST)))
                        GlobalFree((HANDLE) proc->Params[ParamIndex].Input);
                    proc->Params[ParamIndex].Input = temp4;
                }
                if (temp3 == 1)
                    proc->Params[ParamIndex].Output = temp4;
                // Next parameter is output or something else
                temp3++;
            }

            ChangesDone = PCD_DONE;
            break;

        // Options sections parser
        case PST_OPTIONS:
            temp2 = 0;
            switch (*ib)
            {
            case ' ':
                break;
            case '!': temp = -temp; break;
            case 'c':
                temp2 = POPT_CDECL;
                break;
            case 'r':
                temp2 = POPT_ALWRETURN;
                break;
            case 'n':
                temp2 = POPT_NEVERREDEF;
                break;
            case 's':
                temp2 = POPT_GENSTACK;
                break;
            case 'e':
                temp2 = POPT_ERROR;
                break;
            case 'u':
                temp2 = POPT_UNLOAD;
                break;
            }

            // New Options
            if (temp2 != 0)
            {
                if (temp == 1) proc->Options |= temp2;
                else proc->Options &= ~temp2;
                // Back to default (turn on nothing) state
                temp = 1; temp2 = 0;
            }

            ChangesDone = PCD_DONE;
            break;
        }

        // Nothing done, just copy char to buffer
        if (ChangesDone == PCD_NONE) *(cb++) = *(ib);
        // Something done, buffer = ""
        else cb = cbuf;

        // Increase input pointer
        ib++;
    }

    GlobalFree(ibuf);
    GlobalFree(cbuf);
    GlobalFree(sbuf);

    // Ok, the final step: check proc for existance
    if (proc != NULL && proc->Proc == NULL)
    {
        switch (proc->ProcType)
        {
        case PT_NOTHING: break;
        case PT_VTABLEPROC:
            {
                // Use direct system proc address
                int addr;

                proc->Dll = (HANDLE) INT_TO_POINTER(myatoi64(proc->DllName));
  
                if (proc->Dll == 0)
                {
                    proc->ProcResult = PR_ERROR;
                    break;
                }

                addr = (int) proc->Dll;

                // fake-real parameter: for COM interfaces first param is Interface Pointer
                proc->Params[1].Output = IOT_NONE;
                proc->Params[1].Input = (int) AllocStr(proc->DllName);
                proc->Params[1].Size = 1;
                proc->Params[1].Type = PAT_INT;
                proc->Params[1].Option = 0;

                // addr - pointer to interface vtable
                addr = *((int *)addr);
                // now addr contains the pointer to first item at VTABLE
                // add the index of proc
                addr = addr + (int)(myatoi64(proc->ProcName)*4);
                proc->Proc = *((HANDLE*)addr);
            }
            break;
        case PT_PROC:
            if (*proc->DllName == 0)
            {
                // Use direct system proc address
                if ((proc->Proc = (HANDLE) INT_TO_POINTER(myatoi64(proc->ProcName))) == 0)
                    proc->ProcResult = PR_ERROR;
            } else
            {
                // Get DLL address
                if ((proc->Dll = GetModuleHandle(proc->DllName)) == NULL)
                    if ((proc->Dll = LoadLibrary(proc->DllName)) == NULL)
                    {
                        proc->ProcResult = PR_ERROR;
                        break;
                    }

                // Get proc address
                if ((proc->Proc = GetProcAddress(proc->Dll, proc->ProcName)) == NULL)
                {
                    // automatic A discover
                    lstrcat(proc->ProcName, "A");
                    if ((proc->Proc = GetProcAddress(proc->Dll, proc->ProcName)) == NULL)
                        proc->ProcResult = PR_ERROR;                            
                }                    
            }
            break;
        case PT_STRUCT:
            if (*(proc->ProcName) != 0) proc->Proc = (HANDLE) INT_TO_POINTER(myatoi64(proc->ProcName));
            break;
        }
    }

    return proc;
}

void ParamAllocate(SystemProc *proc)
{
    int i;

    for (i = 0; i <= proc->ParamCount; i++)
        if (((HANDLE) proc->Params[i].Value == NULL) && (proc->Params[i].Option == -1))
        {
            proc->Params[i].Value = (int) GlobalAlloc(GPTR, 4*ParamSizeByType[proc->Params[i].Type]);
        }
}

void ParamsIn(SystemProc *proc)
{
    int i, *place;
    char *realbuf;
    LPWSTR wstr;

    i = (proc->ParamCount > 0)?(1):(0);
    while (TRUE)
    {
        ProcParameter *par = &proc->Params[i];
        // Step 1: retrive value
        if ((par->Input == IOT_NONE) || (par->Input == IOT_INLINE)) 
            realbuf = AllocStr("");
        else if (par->Input == IOT_STACK) realbuf = system_popstring();
        else if ((par->Input > 0) && (par->Input <= __INST_LAST)) 
            realbuf = system_getuservariable(par->Input - 1);
        else 
        {
            // Inline input, will be freed as realbuf
            realbuf = (char*) par->Input;
            par->Input = IOT_INLINE;
        }

        // Retreive pointer to place
        if (par->Option == -1) place = (int*) par->Value;
        else place = (int*) &(par->Value);

        // by default no blocks are allocated
        par->allocatedBlock = NULL;

        // Step 2: place it
        switch (par->Type)
        {
        case PAT_VOID:
            par->Value = 0;
            break;
        case PAT_INT:
            *((int*) place) = (int) myatoi64(realbuf);
            break;
        case PAT_LONG:
            *((__int64*) place) = myatoi64(realbuf);
            break;
        case PAT_STRING:
/*            if (par->Input == IOT_NONE) 
                *((int*) place) = (int) NULL;
            else*/
            *((int*) place) = (int) (par->allocatedBlock = AllocStr(realbuf));
            break;
        case PAT_WSTRING:
        case PAT_GUID:
            wstr = (LPWSTR) (par->allocatedBlock = GlobalAlloc(GPTR, g_stringsize*2));
            MultiByteToWideChar(CP_ACP, 0, realbuf, g_stringsize, wstr, g_stringsize);
            if (par->Type == PAT_GUID)
            {
                *((HGLOBAL*)place) = (par->allocatedBlock = GlobalAlloc(GPTR, 16));
                CLSIDFromString(wstr, *((LPCLSID*)place));
                GlobalFree((HGLOBAL) wstr);
            } else
                *((LPWSTR*)place) = wstr;
            break;
        case PAT_CALLBACK:
            // Generate new or use old callback
            if (lstrlen(realbuf) > 0)
                par->Value = (int) CreateCallback((SystemProc*) INT_TO_POINTER(myatoi64(realbuf)));
            break;
        }
        GlobalFree(realbuf);

#ifdef SYSTEM_LOG_DEBUG
        {
            char buf[1024];
            wsprintf(buf, "\t\t\tParam In %d:    type %d value 0x%08X value2 0x%08X\n", i, 
                par->Type, par->Value, par->_value);
            SYSTEM_LOG_ADD(buf);
        }
#endif

        if (i == 0) break;
        if (i == proc->ParamCount) i = 0;
        else i++;
    } 
}

void ParamsDeAllocate(SystemProc *proc)
{
    int i;

    for (i = proc->ParamCount; i >= 0; i--)
        if (((HANDLE) proc->Params[i].Value != NULL) && (proc->Params[i].Option == -1))
        {
            GlobalFree((HANDLE) (proc->Params[i].Value));
            proc->Params[i].Value = (int) NULL;
        }
}

void ParamsOut(SystemProc *proc)
{
    int i, *place;
    char *realbuf;
    LPWSTR wstr;

    i = proc->ParamCount;
    do
    {
        // Retreive pointer to place
        if (proc->Params[i].Option == -1) place = (int*) proc->Params[i].Value;
        else place = (int*) &(proc->Params[i].Value);

        realbuf = AllocString();

        // Step 1: retrive value
        switch (proc->Params[i].Type)
        {
        case PAT_VOID:
            lstrcpy(realbuf,"");
            break;
        case PAT_INT:
            wsprintf(realbuf, "%d", *((int*) place));
            break;
        case PAT_LONG:
            myitoa64(*((__int64*) place), realbuf);
            break;
        case PAT_STRING:
            {
                unsigned num = lstrlen(*((char**) place));
                if (num >= g_stringsize) num = g_stringsize-1;
                lstrcpyn(realbuf,*((char**) place), num+1);
                realbuf[num] = 0;                
            }
            break;
        case PAT_GUID:
            wstr = (LPWSTR) GlobalAlloc(GPTR, g_stringsize*2);
            StringFromGUID2(*((REFGUID*)place), wstr, g_stringsize*2);
            WideCharToMultiByte(CP_ACP, 0, wstr, g_stringsize, realbuf, g_stringsize, NULL, NULL); 
            GlobalFree((HGLOBAL)wstr);
            break;
        case PAT_WSTRING:
            wstr = *((LPWSTR*)place);
            WideCharToMultiByte(CP_ACP, 0, wstr, g_stringsize, realbuf, g_stringsize, NULL, NULL);             
            break;
        case PAT_CALLBACK:
            wsprintf(realbuf, "%d", proc->Params[i].Value);
            break;
        }

        // memory cleanup
        if ((proc->Params[i].allocatedBlock != NULL) && ((proc->ProcType != PT_STRUCT)
            || (proc->Params[i].Option > 0)))
            GlobalFree(proc->Params[i].allocatedBlock);

        // Step 2: place it
        if (proc->Params[i].Output == IOT_NONE);
        else if (proc->Params[i].Output == IOT_STACK) system_pushstring(realbuf);
        else if (proc->Params[i].Output > 0) system_setuservariable(proc->Params[i].Output - 1, realbuf);

        GlobalFree(realbuf);

        i--;
    } 
    while (i >= 0);
}

HANDLE CreateCallback(SystemProc *cbproc)
{
    char *mem;

    if (cbproc->Proc == NULL)
    {
        // Set callback index
        cbproc->CallbackIndex = ++(CallbackIndex);
        cbproc->Options |= POPT_PERMANENT;

        mem = (char *) (cbproc->Proc = VirtualAlloc(NULL, 10, MEM_COMMIT, PAGE_EXECUTE_READWRITE));
        *(mem++) = (char) 0xB8; // Mov eax, const
        *((int *)mem) = (int) cbproc;
        mem += sizeof(int);
        *(mem++) = (char) 0xe9; // Jmp relative
        *((int *)mem) = (int) RealCallBack;
        *((int *)mem) -= ((int) mem) + 4;
    }

    // Return proc address
    return cbproc->Proc;
}

void CallStruct(SystemProc *proc)
{
    BOOL ssflag;
    int i, structsize = 0, size = 0;
    char *st, *ptr;

    SYSTEM_LOG_ADD("\t\tStruct...");

    // Calculate the structure size 
    for (i = 1; i <= proc->ParamCount; i++)
        if (proc->Params[i].Option < 1)
            structsize += proc->Params[i].Size * 4;
        else
            structsize += proc->Params[i].Option-1;
    
    // Struct exists?
    if (proc->Proc == NULL)
        // No. Allocate struct memory
        proc->Proc = (HANDLE) GlobalAlloc(GPTR, structsize);
    else  // In case of zero size defined structure use mapped size 
        if (structsize == 0) structsize = (int) GlobalSize((HANDLE) proc->Proc);
    
    // Pointer to current data
    st = (char*) proc->Proc;

    for (i = 1; i <= proc->ParamCount; i++)
    {
        ssflag = FALSE;

        // Normal or special block?
        if (proc->Params[i].Option < 1)
        {
            // Normal
            size = proc->Params[i].Size*4;
            ptr = (char*) &(proc->Params[i].Value);
        }
        else
        {
            int intmask[4] = {0xFFFFFFFF, 0x000000FF, 0x0000FFFF, 0x00FFFFFF};

            // Special
            size = proc->Params[i].Option-1;

            switch (proc->Params[i].Type)
            {
            case PAT_VOID: ptr = NULL; break;
            case PAT_LONG: 
                // real structure size
                proc->Params[i].Value = structsize;
                proc->Params[i]._value = 0;
                ssflag = TRUE;
            case PAT_INT: 
                // clear unused value bits
                proc->Params[i].Value &= intmask[((size >= 0) && (size < 4))?(size):(0)];
                // pointer
                ptr = (char*) &(proc->Params[i].Value); 
                break;

            case PAT_STRING: 
            case PAT_GUID: 
            case PAT_WSTRING: 
                ptr = (char*) proc->Params[i].Value; break;
            }
        }

        // Process them!
        if (ptr != NULL)
        {
            // Input
            if ((proc->Params[i].Input != IOT_NONE) || (ssflag))
                copymem(st, ptr, size);

            // Output
            if (proc->Params[i].Output != IOT_NONE)
                copymem(ptr, st, size);
        }

        // Skip to next data block
        st += size;
    }

    SYSTEM_LOG_POST;

    // Proc virtual return - pointer to memory struct
    proc->Params[0].Value = (int) proc->Proc;
}

/*
Use of system _DllMainCRTStartup to avoid endless recursion for the debug
report macro _RPT0. 

The system _DllMainCRTStartup initializes the C runtime environment. 
In particular the value for _osplatform is initialized. In the function 
_get_winmajor called in the execution of the _RPT0 macro an assertion 
failure is raised if _osplatform is not set. The assertion is reported by 
the same means as used for the _RPT0 macro. This leads to an endless recursion.
*/

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
        g_hInstance=hInst;
        
        if (ul_reason_for_call == DLL_PROCESS_ATTACH)
        {
            // change the protection of return command
            VirtualProtect(&retexpr, sizeof(retexpr), PAGE_EXECUTE_READWRITE, (PDWORD)&LastStackPlace);

            // initialize some variables
            LastStackPlace = 0;
            LastStackReal = 0;
            LastError = 0;
            LastProc = NULL;
            CallbackIndex = 0;
            retexpr[0] = (char) 0xC2;
            retexpr[2] = 0x00;
        }

        return TRUE;
}

/*
Returns size by which the stack should be expanded
*/
unsigned int GetNewStackSize(void)
{
		return NEW_STACK_SIZE;
}

/*
Returns non-zero value if GENSTACK option is set
*/
unsigned int GetGenStackOption(SystemProc *proc)
{
		return (proc->Options & POPT_GENSTACK);
}

/*
Returns non-zero value if CDECL option is set
*/
unsigned int GetCDeclOption(SystemProc *proc)
{
		return (proc->Options & POPT_CDECL);
}

/*
Returns non-zero value if Error option is set
*/
unsigned int GetErrorOption(SystemProc *proc)
{
		return (proc->Options & POPT_ERROR);
}

/*
Returns offset for element Proc of SystemProc structure
*/
unsigned int GetProcOffset(void)
{
		return (unsigned int)(&(((SystemProc *)0)->Proc));
}

/*
Returns offset for element Clone of SystemProc structure
*/
unsigned int GetCloneOffset(void)
{
		return (unsigned int)(&(((SystemProc *)0)->Clone));
}

/*
Returns offset for element ProcName of SystemProc structure
*/
unsigned int GetProcNameOffset(void)
{
		return (unsigned int)(&(((SystemProc *)0)->ProcName));
}

/*
Returns offset for element ArgsSize of SystemProc structure
*/
unsigned int GetArgsSizeOffset(void)
{
		return (unsigned int)(&(((SystemProc *)0)->ArgsSize));
}

/*
Returns number of parameters
*/
unsigned int GetParamCount(SystemProc *proc)
{
		return proc->ParamCount;
}

/*
Returns offset for element Params of SystemProc structure
*/
unsigned int GetParamsOffset(void)
{
		return (unsigned int)(&(((SystemProc *)0)->Params));
}

/*
Returns size of ProcParameter structure
*/
unsigned int GetSizeOfProcParam(void)
{
		return (sizeof(ProcParameter));
}


/*
Returns offset for element Size of ProcParameter structure
*/
unsigned int GetSizeOffsetParam(void)
{
		return (unsigned int)(&(((ProcParameter *)0)->Size));
}

/*
Returns offset for element Value of ProcParameter structure
*/
unsigned int GetValueOffsetParam(void)
{
		return (unsigned int)(&(((ProcParameter *)0)->Value));
}

/*
Returns offset for element _value of ProcParameter structure
*/
unsigned int Get_valueOffsetParam(void)
{
		return (unsigned int)(&(((ProcParameter *)0)->_value));
}

/*
Sets "CLONE" option
*/
void SetCloneOption(SystemProc *proc)
{
	proc->Options |= POPT_CLONE;
}

/*
Sets Result of procedure call to be "OK"
*/
void SetProcResultOk(SystemProc *proc)
{
	proc->ProcResult = PR_OK;
}

/*
Sets Result of procedure call to be "CALLBACK"
*/
void SetProcResultCallback(SystemProc *proc)
{
	proc->ProcResult = PR_CALLBACK;
}
