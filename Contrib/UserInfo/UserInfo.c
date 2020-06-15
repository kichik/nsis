// Unicode support by Jim Park -- 08/23/2007

#include <windows.h>
#include <nsis/pluginapi.h> // nsis plugin

#if defined(_MSC_VER) && !defined(GetVersion)
#if _MSC_VER >= 1500
FORCEINLINE DWORD NoDepr_GetVersion() { __pragma(warning(push))__pragma(warning(disable:4996)) DWORD r = GetVersion(); __pragma(warning(pop)) return r; }
#define GetVersion NoDepr_GetVersion
#endif //~ _MSC_VER >= 1500
#endif //~ _MSC_VER

typedef BOOL (WINAPI*CHECKTOKENMEMBERSHIP)(HANDLE TokenHandle,PSID SidToCheck,PBOOL IsMember);
CHECKTOKENMEMBERSHIP _CheckTokenMembership=NULL;

void __declspec(dllexport) GetName(HWND hwndParent, int string_size, 
                                   TCHAR *variables, stack_t **stacktop)
{
  EXDLL_INIT();

  {
    DWORD dwStringSize = g_stringsize;
    stack_t *th;
    if (!g_stacktop) return;
    th = (stack_t*) GlobalAlloc(GPTR, sizeof(stack_t) + g_stringsize*sizeof(TCHAR));
    if (!GetUserName(th->text, &dwStringSize)) // Fails with ERROR_NOT_LOGGED_ON on Win9x if you cancel the logon dialog.
    {
      *th->text = _T('\0');
    }
    th->next = *g_stacktop;
    *g_stacktop = th;
  }
}

struct group
{
 DWORD auth_id;
 TCHAR *name;
};

static const struct group groups[] = 
{
 {DOMAIN_ALIAS_RID_USERS, _T("User")},
 // every user belongs to the users group, hence users come before guests
 {DOMAIN_ALIAS_RID_GUESTS, _T("Guest")},
 {DOMAIN_ALIAS_RID_POWER_USERS, _T("Power")},
 {DOMAIN_ALIAS_RID_ADMINS, _T("Admin")}
};

TCHAR* GetAccountTypeHelper(BOOL CheckTokenForGroupDeny) 
{
  TCHAR  *group = NULL;
  HANDLE  hToken = NULL;

/*
  [Marius]
  https://sourceforge.net/p/mingw-w64/bugs/821/
  Sometime in 2019 a change was made to mingw-x86 libraries that would generate code linked to kernel32!OpenThreadToken instead of advapi32!OpenThreadToken.
  Starting with Win7 OpenThreadToken exists in both kernel32.dll and advapi32.dll, but before that it was only present in advapi32.
  We'll call advapi32!OpenThreadToken dynamically to avoid compatibility problems.
  (2020-06, gcc/10.1.0-3)
*/
  typedef BOOL (WINAPI *TOpenThreadToken)( HANDLE ThreadHandle, DWORD DesiredAccess, BOOL OpenAsSelf, PHANDLE TokenHandle );
  TOpenThreadToken fnOpenThreadToken = (TOpenThreadToken)GetProcAddress( GetModuleHandle(_T("advapi32")), "OpenThreadToken");
  if (!fnOpenThreadToken)
    fnOpenThreadToken = (TOpenThreadToken)GetProcAddress( GetModuleHandle(_T("kernel32")), "OpenThreadToken");
  if (!fnOpenThreadToken)
    return _T("Admin");

#ifndef _WIN64
  if (GetVersion() & 0x80000000) // Not NT
  {
    return _T("Admin");
  }
#endif

  // First we must open a handle to the access token for this thread.
  if (fnOpenThreadToken(GetCurrentThread(), TOKEN_QUERY, FALSE, &hToken) ||
    OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &hToken))
  {
    SID_IDENTIFIER_AUTHORITY SystemSidAuthority = {SECURITY_NT_AUTHORITY};
    TOKEN_GROUPS  *ptg          = NULL;
    BOOL       ValidTokenGroups = FALSE;
    DWORD      cbTokenGroups;
    DWORD      i, j;
    
    
    if (CheckTokenForGroupDeny)
      // GetUserName is in advapi32.dll so we can avoid Load/Freelibrary
      _CheckTokenMembership=
      #ifndef _WIN64
        (CHECKTOKENMEMBERSHIP) GetProcAddress(
          GetModuleHandle(_T("ADVAPI32")), "CheckTokenMembership");
      #else
        CheckTokenMembership;
      #endif
    
    // Use "old school" membership check?
    if (!CheckTokenForGroupDeny || _CheckTokenMembership == NULL)
    {
      // We must query the size of the group information associated with
      // the token. Note that we expect a FALSE result from GetTokenInformation
      // because we've given it a NULL buffer. On exit cbTokenGroups will tell
      // the size of the group information.
      if (!GetTokenInformation(hToken, TokenGroups, NULL, 0, &cbTokenGroups) &&
        GetLastError() == ERROR_INSUFFICIENT_BUFFER)
      {
        // Allocate buffer and ask for the group information again.
        // This may fail if an administrator has added this account
        // to an additional group between our first call to
        // GetTokenInformation and this one.
        if ((ptg = GlobalAlloc(GPTR, cbTokenGroups)) &&
          GetTokenInformation(hToken, TokenGroups, ptg, cbTokenGroups, &cbTokenGroups))
        {
          ValidTokenGroups=TRUE;
        }
      }
    }
    
    if (ValidTokenGroups || (CheckTokenForGroupDeny && _CheckTokenMembership))
    {
      PSID psid;
      for (i = 0; i < sizeof(groups)/sizeof(struct group); i++)
      {
        // Create a SID for the local group and then check if it exists in our token
        if (AllocateAndInitializeSid(
          &SystemSidAuthority, 2, SECURITY_BUILTIN_DOMAIN_RID,
          groups[i].auth_id, 0, 0, 0, 0, 0, 0,&psid))
        {
          BOOL IsMember = FALSE;
          if (CheckTokenForGroupDeny && _CheckTokenMembership)
          {
            _CheckTokenMembership(0, psid, &IsMember);
          }
          else if (ValidTokenGroups)
          {
            for (j = 0; j < ptg->GroupCount; j++)
            {
              if (EqualSid(ptg->Groups[j].Sid, psid))
              {
                IsMember = TRUE;
              }
            }
          }
          
          if (IsMember) group=groups[i].name;
          FreeSid(psid);
        }
      }
    }

    if (ptg)
      GlobalFree(ptg);
    CloseHandle(hToken);

    return group;
  }

  return _T("");
}

void __declspec(dllexport) GetAccountType(HWND hwndParent, int string_size, 
                                          TCHAR *variables, stack_t **stacktop)
{
  EXDLL_INIT();
  pushstring(GetAccountTypeHelper(TRUE));
}

void __declspec(dllexport) GetOriginalAccountType(HWND hwndParent, int string_size, 
                                                  TCHAR *variables, stack_t **stacktop)
{
  EXDLL_INIT();
  pushstring(GetAccountTypeHelper(FALSE));
}

BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  return TRUE;
}

