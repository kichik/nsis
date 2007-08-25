#include <windows.h>
#include "../ExDLL/exdll.h"
typedef BOOL (WINAPI*CHECKTOKENMEMBERSHIP)(HANDLE TokenHandle,PSID SidToCheck,PBOOL IsMember);
CHECKTOKENMEMBERSHIP _CheckTokenMembership=NULL;

void __declspec(dllexport) GetName(HWND hwndParent, int string_size, 
                                   char *variables, stack_t **stacktop)
{
  EXDLL_INIT();

  {
    DWORD dwStringSize = g_stringsize;
    stack_t *th;
    if (!g_stacktop) return;
    th = (stack_t*) GlobalAlloc(GPTR, sizeof(stack_t) + g_stringsize);
    GetUserName(th->text, &dwStringSize);
    th->next = *g_stacktop;
    *g_stacktop = th;
  }
}

char* GetAccountTypeHelper(BOOL CheckTokenForGroupDeny) 
{
  char  *group = NULL;
  HANDLE  hToken = NULL;
  struct group
  {
    DWORD auth_id;
    char *name;
  };

  struct group groups[] = 
  {
    {DOMAIN_ALIAS_RID_USERS, "User"},
    // every user belongs to the users group, hence users come before guests
    {DOMAIN_ALIAS_RID_GUESTS, "Guest"},
    {DOMAIN_ALIAS_RID_POWER_USERS, "Power"},
    {DOMAIN_ALIAS_RID_ADMINS, "Admin"}
  };

  if (GetVersion() & 0x80000000) // Not NT
  {
    return "Admin";
  }

  // First we must open a handle to the access token for this thread.
  if (OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, FALSE, &hToken) ||
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
        (CHECKTOKENMEMBERSHIP) GetProcAddress(
          GetModuleHandle("ADVAPI32"), "CheckTokenMembership");
    
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

  return "";
}

void __declspec(dllexport) GetAccountType(HWND hwndParent, int string_size, 
                                          char *variables, stack_t **stacktop)
{
  EXDLL_INIT();
  pushstring(GetAccountTypeHelper(TRUE));
}

void __declspec(dllexport) GetOriginalAccountType(HWND hwndParent, int string_size, 
                                                  char *variables, stack_t **stacktop)
{
  EXDLL_INIT();
  pushstring(GetAccountTypeHelper(FALSE));
}

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  return TRUE;
}

