#include <windows.h>
#include "..\exdll\exdll.h"

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

void __declspec(dllexport) GetAccountType(HWND hwndParent, int string_size, 
                                          char *variables, stack_t **stacktop)
{
  EXDLL_INIT();

  {
    HANDLE        hThread;
    TOKEN_GROUPS  *ptg = NULL;
    DWORD         cbTokenGroups;
    DWORD         i, j;

    SID_IDENTIFIER_AUTHORITY SystemSidAuthority = SECURITY_NT_AUTHORITY;

    char *group = "";

    if (GetVersion() & 0x80000000) // Not NT
    {
      group = "Admin";
    }

    // First we must open a handle to the access token for this thread.

    else if (OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, FALSE, &hThread) ||
        OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &hThread))
    {
      // Then we must query the size of the group information associated with
      // the token. Note that we expect a FALSE result from GetTokenInformation
      // because we've given it a NULL buffer. On exit cbTokenGroups will tell
      // the size of the group information.

      if (!GetTokenInformation (hThread, TokenGroups, NULL, 0, &cbTokenGroups) &&
          GetLastError() == ERROR_INSUFFICIENT_BUFFER)
      {

        // Now we allocate a buffer for the group information.
        // Since _alloca allocates on the stack, we don't have
        // to explicitly deallocate it. That happens automatically
        // when we exit this function.

        if (ptg = GlobalAlloc(GPTR, cbTokenGroups))
        {

          // Now we ask for the group information again.
          // This may fail if an administrator has added this account
          // to an additional group between our first call to
          // GetTokenInformation and this one.

          if (GetTokenInformation(hThread, TokenGroups, ptg, cbTokenGroups, &cbTokenGroups))
          {

            struct group
            {
              DWORD auth_id;
              char *name;
            } groups[] = {
              {DOMAIN_ALIAS_RID_USERS, "User"},
              // every user belongs to the users group, hence users comes before guests
              {DOMAIN_ALIAS_RID_GUESTS, "Guest"},
              {DOMAIN_ALIAS_RID_POWER_USERS, "Power"},
              {DOMAIN_ALIAS_RID_ADMINS, "Admin"}
            };

            // Finally we'll iterate through the list of groups for this access
            // token looking for a match against the SID we created above.

            for (i = 0; i < sizeof(groups)/sizeof(struct group); i++)
            {
              PSID psid = 0;
              AllocateAndInitializeSid(
                &SystemSidAuthority,
                2,
                SECURITY_BUILTIN_DOMAIN_RID,
                groups[i].auth_id,
                0, 0, 0, 0, 0, 0,
                &psid
              );
              if (psid == 0) continue;
              for (j = 0; j < ptg->GroupCount; j++)
                if (EqualSid(ptg->Groups[j].Sid, psid))
                  group = groups[i].name;
              FreeSid(psid);
            }
          }

          GlobalFree(ptg);
        }
      }

      CloseHandle(hThread);
    }

    pushstring(group);
  }
}

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
	return TRUE;
}
